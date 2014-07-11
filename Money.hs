module Money
    (
      Transaction(..)
    , JUTCTime(..)
    , similarTransactions
    , fuzzyMatchChecks
    , monthStats
    , tag
    , month
    )
    where

import Control.Applicative ((<$>))
import Data.Function (on)
import Data.List (tails, sortBy, groupBy, foldl')
import qualified Data.Map as Map (Map, unionWith, map, intersectionWith, fromList, toList, fromListWith)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Time (UTCTime)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (utctDay)
import Data.Time.Format (parseTime, formatTime)
import Util (listApp, count, median)
import System.Locale (defaultTimeLocale)
import Text.JSON (Result(..), JSValue(..), JSON(..), toJSObject, fromJSObject, toJSString, fromJSString)

data Transaction = Transaction { description :: String
                               , date :: JUTCTime
                               , amount :: Int
                               , tags :: [String]
                               } deriving (Show, Eq, Ord)

tag :: Transaction -> String
tag = fromMaybe "" . listToMaybe . tags

month :: Transaction -> Int
month = (\(_,m,_) -> m) . toGregorian . utctDay . runJUTCTime . date

-- Database.CouchDB forces us to use Text.JSON tsk tsk
-- with Aeson none of this instance code is required
-- though it "could" be cleaned up
instance JSON Transaction where
    -- It may not be immediately obvious that if any of the jslookup's fail the whole
    -- thing will return an Error because of the monad instance of Result
    readJSON (JSObject o) = let l = fromJSObject o
        in do desc <- jslookup "description" l
              dt <- jslookup "date" l
              amt <- jslookup "amount" l
              tg <- jslookup "tags" l
              return Transaction { description = desc
                                 , date = dt
                                 , amount = amt
                                 , tags = tg
                                 }
            where
                jslookup k l = maybe (Error $ "missing key: " ++ k) readJSON (lookup k l)
    readJSON _ = Error "Not an object"
    showJSON t = showJSON $ toJSObject [
                                          ("description", showJSON $ description t)
                                        , ("date", showJSON $ date t)
                                        , ("amount", showJSON $ amount t)
                                        , ("tags", showJSONs $ tags t)
                                       ]

newtype JUTCTime = JUTCTime { runJUTCTime :: UTCTime } deriving (Eq, Ord)
instance Show JUTCTime where
    show (JUTCTime a) = formatTime defaultTimeLocale "%m/%d/%Y" a

instance JSON JUTCTime where
    readJSON (JSString s) = maybeToResult "parseTime Failed" $ JUTCTime <$> parseTime defaultTimeLocale "%m/%d/%Y" (fromJSString s)
        where
            maybeToResult :: String -> Maybe a -> Result a
            maybeToResult _ (Just a) = Ok a
            maybeToResult st Nothing = Error st
    readJSON _ = Error "not string"
    showJSON d = showJSON $ toJSString $ formatTime defaultTimeLocale "%m/%d/%Y" (runJUTCTime d)

{-
    fuzzy string matching for transaction descriptions
    works as follows:
        "this is string 1" "here is another string"
        ["this", "is", ...] ["here", "is", ...]
        [["this", "is", "string", "1"], ["is", "string", "1"], ["string", "1"], ...] [["here", ...], ...]

        now these 2 lists of tails of words of the strings are matched against each other

        (["this", "is", "string", "1"] `match` ["here", "is", "another", "string"]) +
        (["this", "is", "string", "1"] `match` ["is", "another", "string"]) +

        this sum is then normalized by n * (n + 1)/2 where n is the length of the larger string
    ideas:
        also include inits in permutation and then replace match with (==)
-}
fuzzyMatch :: String -> String -> Double
fuzzyMatch a b = fromIntegral countEqual/normalizer
    where
        maxLen = fromIntegral $ max (length (words a)) (length (words b))
        pluralize = tails . words
        permutations = mapM pluralize [a, b]
        countEqual = (sum . map (listApp match)) permutations
        match as bs = count True $ zipWith (==) as bs
        -- figured this out by trial and error. this is n*(n+1)/2 which is sum (map length (tails (words a)))
        normalizer = maxLen * (maxLen + 1) * 0.5

fuzzyMatchChecks :: [Bool]
fuzzyMatchChecks = [ fuzzyMatch "hello world" "hello world" > fuzzyMatch "hello world" ""
                   , fuzzyMatch "hello world" "hello world" > fuzzyMatch "hello world" "else world"
                   , fuzzyMatch "hello world" "hello world" > fuzzyMatch "hello world" "world hello"
                   , fuzzyMatch "hello world" "hello world" > fuzzyMatch "hello world" "hello hello"
                   , fuzzyMatch "hello world" "hello world" > fuzzyMatch "hello world" "hello world hello world"
                   , fuzzyMatch "hello world" "hello world" == fuzzyMatch "hello world a b" "hello world a b"
                   , fuzzyMatch "hello world" "hello fail" < fuzzyMatch "hello world a b" "hello fail a b"
                   , fuzzyMatch "hello world" "hello world" == fuzzyMatch "hello hello" "hello hello"
                   ]

-- TODO use Arrows ?
similarTransactions :: Transaction -> [Transaction] -> [(Double, Transaction)]
similarTransactions t = sortBy (flip compare `on` fst) . map (\x -> (score x, x))
    where
        score x = fuzzyMatch (description x) (description t)

monthStats :: [Transaction] -> [(String, Double)]
monthStats ts = Map.toList $ Map.map median $ foldl' (Map.intersectionWith (flip (:))) (emptyTagMapList ts) $ map (Map.unionWith (+) (emptyTagMap ts) . amountPerTag) (tsByMonth ts)
    where
        emptyTagMapList :: [Transaction] -> Map.Map String [Double]
        emptyTagMapList = Map.fromList . map (\a -> (tag a, []))
        emptyTagMap = Map.fromList . map (\a -> (tag a, 0))
        tsByMonth = groupBy ((==) `on` month) . sortBy (compare `on` date)
        amountPerTag = Map.fromListWith (+) . map transactionToPair
        transactionToPair t = (tag t, fromIntegral $ amount t)
