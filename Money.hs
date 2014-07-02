module Money
    (
      Transaction(..)
    , similarTransactions
    , sortAndGroup
    , fuzzyMatchChecks
    , monthStats
    )
    where

import Control.Applicative ((<$>))
import Data.Function (on)
import Data.List (sort, tails, sortBy, groupBy, foldl')
import qualified Data.Map as Map (Map, unionWith, map, intersectionWith, fromList, toList, fromListWith)
import qualified Data.Set as Set (fromList)
import Data.Time (UTCTime)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (utctDay)
import Data.Time.Format (parseTime, formatTime)
import Util (listApp, count)
import System.Locale (defaultTimeLocale)
import Text.JSON (Result(..), JSValue(..), JSON(..), toJSObject, fromJSObject, toJSString, fromJSString)

data Transaction = Transaction { description :: String
                               , date :: JUTCTime
                               , amount :: Int
                               , tags :: [String]
                               } deriving (Show, Eq, Ord)

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

newtype JUTCTime = JUTCTime { runJUTCTime :: UTCTime } deriving (Show, Eq, Ord)
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

sortAndGroup :: [Transaction] -> [[[Transaction]]]
sortAndGroup = map sortGroupTags . sortGroupMonth
    where
        sortGroupMonth :: [Transaction] -> [[Transaction]]
        sortGroupMonth = groupBy ((==) `on` month) . sortBy (flip compare `on` date)
        sortGroupTags :: [Transaction] -> [[Transaction]]
        sortGroupTags = groupBy ((==) `on` (Set.fromList . tags)) . sortBy (compare `on` (Set.fromList . tags))

headDef :: a -> [a] -> a
headDef d [] = d
headDef _ (x:_) = x

tag :: Transaction -> String
tag = headDef "" . tags

month :: Transaction -> Int
month = (\(_,m,_) -> m) . toGregorian . utctDay . runJUTCTime . date

mean :: Floating a => [a] -> a
mean = fst . foldl' (\(m, n) x -> (m+(x-m)/(n+1),n+1)) (0,0)

median :: (Floating a, Ord a) => [a] -> a
median x | odd n  = head  $ drop (n `div` 2) x'
         | even n = mean $ take 2 $ drop i x'
         | otherwise = undefined -- This never happens but not including this case causes non-exhaustive warning. I wonder if this can be solved in dependently typed languages.
                  where i = (length x' `div` 2) - 1
                        x' = sort x
                        n  = length x

monthStats :: [Transaction] -> [(String, Double)]
monthStats ts = Map.toList $ Map.map median $ foldl' (Map.intersectionWith (flip (:))) (emptyTagMapList ts) $ map (Map.unionWith (+) (emptyTagMap ts) . amountPerTag) (tsByMonth ts)
    where
        emptyTagMapList :: [Transaction] -> Map.Map String [Double]
        emptyTagMapList = Map.fromList . map (\a -> (tag a, []))
        emptyTagMap = Map.fromList . map (\a -> (tag a, 0))
        tsByMonth = groupBy ((==) `on` month) . sortBy (compare `on` date)
        amountPerTag = Map.fromListWith (+) . map transactionToPair
        transactionToPair t = (tag t, fromIntegral $ amount t)
