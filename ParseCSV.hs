-- Maybe this should be Money.ParseCSV ?
module ParseCSV
    (
      debitCardCsvToTransactions
    )
    where

import Control.Applicative ((<$>))
import Data.Char (isSpace)
import Data.List (elemIndices, isInfixOf)
import Data.Maybe (mapMaybe)
import Data.Time (UTCTime(..))
import Data.Time.Format (parseTime)
import Money (Transaction(..))
import System.Locale (defaultTimeLocale)
import Util ((?), maybeRead, splitOnIndices, count, index)

--TODO generalize,parameterize ?
splitOnNonEscapedCommas :: String -> [String]
splitOnNonEscapedCommas str = splitOnIndices splitIndices str
    where
        isEscapedCommaIndex idx = odd (count '"' $ take idx str)
        splitIndices = filter (not . isEscapedCommaIndex) (elemIndices ',' str)

-- Converts bank of america debit card csv output into [Transaction] Type
-- TODO cleanup var names
debitCardCsvToTransactions :: String -> [Transaction]
debitCardCsvToTransactions s = mapMaybe csvRecordToTransaction tRecords
    where
        tRecords = tail csvRecords
        csvRecords = map splitOnNonEscapedCommas $ tail $ dropWhile (not . all isSpace) $ lines s
        -- this code assumes a particular csv header "Date,Description,Amount,Running Bal."
        csvRecordToTransaction :: [String] -> Maybe Transaction
        csvRecordToTransaction t = do
            _date <- getTime =<< t `index` 0
            _description <- t `index` 1
            _amount <- getAmount =<< t `index` 2
            return Transaction { description = _description
                               , date = _date
                               , amount = _amount
                               , tags = []
                               }
        getAmount :: String -> Maybe Int
        getAmount a = ((ceiling :: Float -> Int) . (*100)) <$> (maybeRead $ filter (/='"') a)
        getTime :: String -> Maybe UTCTime
        getTime = parseTime defaultTimeLocale "%m/%d/%Y"

transactions :: IO [Transaction]
transactions = debitCardCsvToTransactions <$> readFile "/home/karshan/gits/money/stmt.csv"

-- The point of this function is to convert transaction descriptions into
-- usefully unique transactions description. Eventually want something like
-- "CHECKCARD 0401 Chipotle Mountain View CA 12334314123" ->
-- {type: CHECKCARD, date: 0401, name: Chipotle, location: ..., transaction_id: ...}
usefulDescription :: String -> String
usefulDescription s =
    "CHECKCARD" `isInfixOf` head (words s) ? checkCardDescription s $
    s
    where
        checkCardDescription st = unwords $ init $ drop 2 $ clean (words st)
            where
                clean ws = if "RECURRING" `isInfixOf` last ws then init ws else ws
