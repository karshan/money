module Main where

import Control.Applicative ((<$>))
--import Control.Arrow (arr, (&&&))
import Data.Char (isSpace)
import Data.Function (on)
import Data.List (elemIndices, isInfixOf, genericLength, genericTake, genericDrop, tails, sortBy)

data Transaction = Transaction { description :: String
							   , date :: String
							   , amount :: String
							   , runningBalance :: String
							   , tags :: [String]
							   } deriving (Show, Eq)

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

--TODO rename to |>
--TODO pick one $ or & don't use both....
(&) :: a -> (a -> b) -> b
(&) = flip ($)

(?) :: Bool -> a -> a -> a
(?) True  x _ = x
(?) False _ y = y

--unsplit :: (b -> c -> d) -> (b, c) -> d
--unsplit = arr . uncurry

listApp :: (a -> a -> b) -> [a] -> b
listApp f ls = f (head ls) (last ls)

mapTail :: (a -> a) -> [a] -> [a]
mapTail f xs = head xs:map f (tail xs)

window :: (Integral n) => n -> [a] -> [[a]]
window _ [] = []
window n xs = if genericLength xs < n then [] else genericTake n xs:window n (tail xs)

substr :: (Integral n) => n -> n -> [a] -> [a]
substr l u = genericTake (u - l) . genericDrop l

count :: (Eq a, Integral n) => a -> [a] -> n
count x = genericLength . filter (==x)

--TODO generalize String to [a]
--TODO flip some burgers and get rid of the lambda
--WTF (2 :: Int) are you fucking kidding me....... warning otherwise....
splitOnIndices :: (Integral n) => [n] -> [a] -> [[a]]
splitOnIndices is xs = window (2 :: Int) is & map (\a -> listApp substr a xs) & mapTail (genericDrop (1 :: Int))

--TODO generalize ?
--WTF :: Int.... warning otherwise. need to find better workaround
splitOnNonEscapedCommas :: String -> [String]
splitOnNonEscapedCommas str = splitOnIndices splitIndices str
	where
		isEscapedCommaIndex idx = odd ((count '"' $ genericTake idx str) :: Int)
		splitIndices = 0:filter (not . isEscapedCommaIndex) (elemIndices ',' str) ++ [length str]

-- TODO cleanup var names
csvToTransactions :: String -> [Transaction]
csvToTransactions s = map csvRecordToTransaction tRecords
	where 
		tRecords = tail csvRecords
		csvRecords = lines s & dropWhile (not . all isSpace) & tail & map splitOnNonEscapedCommas
		-- TODO make this safe, use header = head csvRecords; maybe ?
		csvRecordToTransaction t = Transaction {description=t !! 1, date=head t, amount=t !! 2, runningBalance=t !! 3, tags=[]}  

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

--fuzzy string matching for descriptions
--matches words independently
--there are warnings here...
fuzzyMatch :: String -> String -> Double
fuzzyMatch a b = fromIntegral countEqual/normalizer
	where
		maxLen = max (genericLength (words a)) (genericLength (words b))
		pluralize = tails . words
		permutations = mapM pluralize [a, b]
		countEqual = (sum . map (listApp innerCount)) permutations :: Int
		innerCount as bs = count True $ zipWith (==) as bs
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

main :: IO ()
main = transactions >>= print

