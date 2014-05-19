module Money
    (
      Transaction(..)
    , similarTransactions
    )
    where

import Data.Function (on)
import Data.List (tails, sortBy)
import Data.Time (UTCTime)
import Data.Time.Format (parseTime, formatTime)
import Util ((&), listApp, count)
import System.Locale (defaultTimeLocale)
import Text.JSON (Result(..), JSValue(..), JSON(..), toJSObject, fromJSObject, toJSString, fromJSString)

data Transaction = Transaction { description :: String
                               , date :: UTCTime
                               , amount :: Int
                               , tags :: [String]
                               } deriving (Show, Eq)

-- Database.CouchDB forces us to use Text.JSON tsk tsk
-- with Aeson none of this instance code is required
-- though it "could" be cleaned up
instance JSON Transaction where
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

instance JSON UTCTime where
    readJSON (JSString s) = maybeToResult "parseTime Failed" $ parseTime defaultTimeLocale "%m/%d/%Y" (fromJSString s)
        where 
            maybeToResult :: String -> Maybe a -> Result a
            maybeToResult _ (Just a) = Ok a
            maybeToResult st Nothing = Error st
    readJSON _ = Error "not string" 
    showJSON d = showJSON $ toJSString $ formatTime defaultTimeLocale "%m/%d/%Y" d

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

similarTransactions :: [Transaction] -> Transaction -> [(Double, Transaction)]
similarTransactions ts t = map (\x -> (score x, x)) ts & sortBy (compare `on` fst) & reverse
    where
        score x = fuzzyMatch (description x) (description t) 
