module ParseCSV
    (
      debitCardCsvToTransactions
    )
    where

import Data.Char (isSpace, isNumber)
import Data.List (elemIndices, isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime(..), Day(..), secondsToDiffTime)
import Data.Time.Format (parseTime)
import Money (Transaction(..))
import System.Locale (defaultTimeLocale)
import Util ((<&>), (&), (?), maybeRead, splitOnIndices, count)

--TODO generalize,parameterize ?
splitOnNonEscapedCommas :: String -> [String]
splitOnNonEscapedCommas str = splitOnIndices splitIndices str
    where
        isEscapedCommaIndex idx = odd (count '"' $ take idx str)
        splitIndices = filter (not . isEscapedCommaIndex) (elemIndices ',' str)

-- Converts bank of america debit card csv output into [Transaction] Type
-- TODO cleanup var names
debitCardCsvToTransactions :: String -> [Transaction]
debitCardCsvToTransactions s = map csvRecordToTransaction tRecords
    where 
        tRecords = tail csvRecords
        csvRecords = lines s & dropWhile (not . all isSpace) & tail & map splitOnNonEscapedCommas
        -- TODO make this safe, use header = head csvRecords; maybe ?
        csvRecordToTransaction t = Transaction {
                                                  description = t !! 1
                                                , date = getTime (head t)
                                                , amount = getAmount (t !! 2)
                                                , tags = []
                                               }
        getAmount a = ceiling $ (*100) $ fromMaybe (0.0 :: Double) $ maybeRead $ filter (\x -> isNumber x || x == '-' || x == '.') a
        getTime a = (fromMaybe defaultTime $ parseTime defaultTimeLocale "%m/%d/%Y" a) :: UTCTime
        defaultTime = UTCTime { utctDay = ModifiedJulianDay 0, utctDayTime = secondsToDiffTime 0 }

-- This should grab the current list of transactions from a database
transactions :: IO [Transaction]
transactions = readFile "/home/karshan/gits/money/stmt.csv" <&> debitCardCsvToTransactions

-- The point of this function is to convert transaction descriptions into
-- usefully unique transactions description. Eventually want something like
-- "CHECKCARD 0401 Chipotle Mountain View CA 12334314123" -> 
-- {type: CHECKCARD, date: 0401, name: Chipotle, location: ..., transaction_id: ...}
usefulDescription :: String -> String
usefulDescription s =
    "CHECKCARD" `isInfixOf` head (words s) ? checkCardDescription s $
    s
    where
        checkCardDescription st = clean (words st) & drop 2 & init & unwords
            where
                clean ws = if "RECURRING" `isInfixOf` last ws then init ws else ws


