{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Money
    (
      Transaction(..)
    , similarTransactions
    , fuzzyMatchChecks
    , monthStats
    , tag
    , month
    )
    where

import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Function      (on)
import           Data.List          (foldl', groupBy, sortBy, tails)
import qualified Data.Map           as Map (Map, fromList, fromListWith,
                                            intersectionWith, map, toList,
                                            unionWith)
import           Data.Maybe         (fromMaybe, listToMaybe)
import           Data.Time          (Day)
import           Data.Time.Calendar (toGregorian)
import           GHC.Generics       (Generic)
import           Util               (count, listApp, median)

data Transaction = Transaction { description :: String
                               , date        :: Day
                               , amount      :: Int
                               , tags        :: [String]
                               } deriving (Show, Read, Eq, Ord, Generic, FromJSON, ToJSON)

tag :: Transaction -> String
tag = fromMaybe "" . listToMaybe . tags

month :: Transaction -> Int
month = (\(_,m,_) -> m) . toGregorian . date

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
