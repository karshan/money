module Main where

import Control.Applicative ((<$>))
import Data.List (elemIndices)
import Data.List.Split (splitOn)

data Transaction = Transaction { description :: String
							   , date :: String
							   , amount :: String
							   , runningBalance :: String
							   , tags :: [String]
							   } deriving (Show)

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

(&) :: a -> (a -> b) -> b
(&) = flip ($)

window :: Int -> [a] -> [[a]]
window _ [] = []
window n xs = if length xs < n then [] else (take n xs):window n (tail xs)

substr :: Int -> Int -> String -> String
substr l u = take (u-l) . drop l 

splitOnIndices :: [Int] -> String -> [String]
splitOnIndices is st = (head out):map (drop 1) (tail out)
	where
		out = map (\a -> substr (head a) (last a) st) $ window 2 is

splitOnNonEscapedCommas :: String -> [String]
splitOnNonEscapedCommas s = splitOnIndices splitIndices s
	where
		isEscapedCommaIndex idx = take idx s & count '"' & odd
		count elt st = filter (==elt) st & length
		splitIndices = 0:(filter (not . isEscapedCommaIndex) (elemIndices ',' s)) ++ [length s]

csvToTransactions :: String -> [Transaction]
csvToTransactions s = map (\t -> Transaction {description=(t !! 1), date=(t !! 0), amount=(t !! 2), runningBalance=(t !! 3), tags=[]}) tRecords
	where 
		header = head csvRecords
		tRecords = tail csvRecords
		csvRecords = lines s & splitOn [""] & last & map splitOnNonEscapedCommas

--TKTK
transactions :: IO [Transaction]
transactions = readFile "C:\\users\\angad\\desktop\\stmt.csv" <&> csvToTransactions

main :: IO ()
main = transactions >>= print