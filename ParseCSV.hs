-- Maybe this should be Money.ParseCSV ?
module ParseCSV
    (
      parseDebit
    , parseCredit
    , parseTCF
    , usefulDescription
    )
    where

import Control.Applicative ((<$>), (<*>))
import Data.Char (isSpace)
import Data.List (elemIndices, isPrefixOf, isInfixOf)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Time (UTCTime(..))
import qualified Data.Time.Format as TF (parseTime)
import Money (Transaction(..), JUTCTime(..))
import System.Locale (defaultTimeLocale)
import Util ((?), maybeRead, splitOnIndices, count, index)

--TODO reimplement with Parsec

--TODO generalize,parameterize ?
splitOnNonEscapedCommas :: String -> [String]
splitOnNonEscapedCommas str = splitOnIndices splitIndices str
    where
        isEscapedCommaIndex idx = odd (count '"' $ take idx str)
        splitIndices = filter (not . isEscapedCommaIndex) (elemIndices ',' str)

parseAmount :: String -> Maybe Int
parseAmount a = ceiling . (* 100) <$> (maybeRead :: String -> Maybe Double) (filter (/='"') a)
parseTime :: String -> String -> Maybe UTCTime
parseTime = TF.parseTime defaultTimeLocale

-- Converts bank of america debit card csv output into [Transaction] Type
-- TODO cleanup var names
parseDebit :: String -> [Transaction]
parseDebit s = mapMaybe csvRecordToTransaction tRecords
    where
        tRecords = tail csvRecords
        csvRecords = map splitOnNonEscapedCommas $ tail $ dropWhile (not . all isSpace) $ lines s
        -- this code assumes a particular csv header "Date,Description,Amount,Running Bal."
        csvRecordToTransaction :: [String] -> Maybe Transaction
        csvRecordToTransaction t = do
            _date <- parseTime "%m/%d/%Y" =<< t `index` 0
            _description <- t `index` 1
            _amount <- parseAmount =<< t `index` 2
            return Transaction { description = _description
                               , date = JUTCTime _date
                               , amount = _amount
                               , tags = []
                               }

parseCredit :: String -> [Transaction]
parseCredit s = mapMaybe recordToTransaction records
    where
        records = tail $ map splitOnNonEscapedCommas $ lines s
        -- this code assumes a particular csv header "Posted Date,Reference Number,Payee,Address,Amount"
        recordToTransaction :: [String] -> Maybe Transaction
        recordToTransaction t = do
            _date <- parseTime "%m/%d/%Y" =<< t `index` 0
            _description <- (++) <$> t `index` 1 <*> ((:) ' ' <$> t `index` 2)
            _amount <- parseAmount =<< t `index` 4
            return Transaction { description = _description
                               , date = JUTCTime _date
                               , amount = _amount
                               , tags = []
                               }

parseTCF :: String -> [Transaction]
parseTCF s = mapMaybe csvRecordToTransaction tRecords
    where
        tRecords = tail csvRecords
        csvRecords = map splitOnNonEscapedCommas $ tail $ dropWhile (not . ("CHECKING" `isPrefixOf`)) $ lines s
        -- this code assumes a particular csv header "Date,Description,Amount,Running Bal."
        csvRecordToTransaction :: [String] -> Maybe Transaction
        csvRecordToTransaction t = do
            _date <- (parseTime "%Y-%m-%d" . head . splitOn " ") =<< t `index` 0
            _description <- t `index` 1
            _isDebit <- (== "Debit") <$> t `index` 8
            _amount <- (\a -> if _isDebit then negate a else a) <$> (parseAmount =<< t `index` 2)
            return Transaction { description = _description
                               , date = JUTCTime _date
                               , amount = _amount
                               , tags = []
                               }

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
