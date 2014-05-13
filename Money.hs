{-# LANGUAGE DeriveGeneric #-}

module Money
    (
      Transaction(..)
    , transactions
    , similarTransactions
    )
    where

import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isSpace, isNumber)
import Data.Function (on)
import Data.List (elemIndices, isInfixOf, tails, sortBy)
import Data.Maybe (fromMaybe, listToMaybe)
import GHC.Generics (Generic)

data Transaction = Transaction { description :: String
                               , date :: String
                               , amount :: Int
                               , runningBalance :: String
                               , tags :: [String]
                               } deriving (Show, Eq, Generic)
instance FromJSON Transaction
instance ToJSON Transaction

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

--TODO rename to |>
--TODO pick one $ or & don't use both....
(&) :: a -> (a -> b) -> b
(&) = flip ($)

(?) :: Bool -> a -> a -> a
(?) True  x _ = x
(?) False _ y = y

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

listApp :: (a -> a -> b) -> [a] -> b
listApp f ls = f (head ls) (last ls)

mapTail :: (a -> a) -> [a] -> [a]
mapTail f xs = head xs:map f (tail xs)

window :: Int -> [a] -> [[a]]
window _ [] = []
window n xs = if length xs < n then [] else take n xs:window n (tail xs)

substr :: Int -> Int -> [a] -> [a]
substr l u = take (u - l) . drop l

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (==x)

--TODO flip some burgers and get rid of the lambda
splitOnIndices :: [Int] -> [a] -> [[a]]
splitOnIndices is xs = window 2 is & map (\a -> listApp substr a xs) & mapTail (drop (1 :: Int))

--TODO generalize,parameterize ?
splitOnNonEscapedCommas :: String -> [String]
splitOnNonEscapedCommas str = splitOnIndices splitIndices str
    where
        isEscapedCommaIndex idx = odd (count '"' $ take idx str)
        splitIndices = 0:filter (not . isEscapedCommaIndex) (elemIndices ',' str) ++ [length str]

-- TODO cleanup var names
csvToTransactions :: String -> [Transaction]
csvToTransactions s = map csvRecordToTransaction tRecords
    where 
        tRecords = tail csvRecords
        csvRecords = lines s & dropWhile (not . all isSpace) & tail & map splitOnNonEscapedCommas
        -- TODO make this safe, use header = head csvRecords; maybe ?
        csvRecordToTransaction t = Transaction {description=t !! 1, date=head t, amount=getAmount (t !! 2), runningBalance=t !! 3, tags=[]}
        getAmount a = ceiling $ (*100) $ fromMaybe 0.0 $ maybeRead $ filter (\x -> isNumber x || x == '-' || x == '.') a

--TKTK
transactions :: IO [Transaction]
transactions = readFile "/Users/karshan/gits/money/stmt.csv" <&> csvToTransactions

--Eventually to be used to identify transactions that are the similar and give the same tags to them
--Maybe a better way to do this would be fuzzy matching
usefulDescription :: String -> String
usefulDescription s =
    "CHECKCARD" `isInfixOf` head (words s) ? checkCardDescription s $
    s
    where
        checkCardDescription st = clean (words st) & drop 2 & init & unwords
            where
                clean ws = if "RECURRING" `isInfixOf` last ws then init ws else ws

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


