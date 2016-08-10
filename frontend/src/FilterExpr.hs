module FilterExpr where

import Control.Lens (over, _Left)
import Control.Monad
import Data.Char
import Data.List
import Data.Time (Day(..))
import Money.API
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Char

data FilterExpr =
    Desc DescExpr
  | Amt AmtExpr
  | Date DateExpr
  | And FilterExpr FilterExpr
  | Or FilterExpr FilterExpr
  | Not FilterExpr
      deriving (Eq, Ord, Show)

data DescExpr =
    CiContains String
      deriving (Eq, Ord, Show)

data AmtExpr =
    AmtLessThan Int
  | AmtGreaterThan Int
  | AmtEqualTo Int
      deriving (Eq, Ord, Show)

data DateExpr =
    DateLessThan Day
  | DateGreaterThan Day
  | DateEqualTo Day
      deriving (Eq, Ord, Show)

evalFilterExpr :: FilterExpr -> Transaction -> Bool
evalFilterExpr (Desc d) t = evalDescExpr d (description t)
evalFilterExpr (Amt d) t = evalAmtExpr d (amount t)
evalFilterExpr (Date d) t = evalDateExpr d (date t)
evalFilterExpr (And e1 e2) t = (evalFilterExpr e1 t) && (evalFilterExpr e2 t)
evalFilterExpr (Or e1 e2) t = (evalFilterExpr e1 t) || (evalFilterExpr e2 t)
evalFilterExpr (Not e) t = not $ evalFilterExpr e t

evalDescExpr :: DescExpr -> String -> Bool
evalDescExpr (CiContains s) desc =
  let s' = map toLower s
      desc' = map toLower desc
  in s' `isInfixOf` desc'

evalAmtExpr :: AmtExpr -> Int -> Bool
evalAmtExpr (AmtLessThan n) amt = amt < n
evalAmtExpr (AmtGreaterThan n) amt = amt > n
evalAmtExpr (AmtEqualTo n) amt = amt == n

evalDateExpr :: DateExpr -> Day -> Bool
evalDateExpr = undefined

spaces' :: Parsec String () ()
spaces' = space >> spaces

parseFilterExpr :: String -> Either String FilterExpr
parseFilterExpr s =
  over _Left show $ parse (spaces *> filterExprParser <* spaces <* eof) "" s

filterExprParser' :: Parsec String () FilterExpr
filterExprParser' =
  (Desc <$> descParser) <|>
  (Amt <$> amtParser)

filterExprParser :: Parsec String () FilterExpr
filterExprParser =
  try (do
    e1 <- filterExprParser'
    spaces'
    string "and"
    spaces'
    e2 <- filterExprParser
    return $ And e1 e2) <|>
  try (do
    e1 <- filterExprParser'
    spaces'
    string "or"
    spaces'
    e2 <- filterExprParser
    return $ Or e1 e2) <|>
  (Not <$> (string "not" *> spaces *> filterExprParser)) <|>
  filterExprParser'

amtParser :: Parsec String () AmtExpr
amtParser = do
  string "amount"
  spaces'
  op <- amtOpParser
  spaces'
  int <- intParser
  return $ op int

amtOpParser :: Parsec String () (Int -> AmtExpr)
amtOpParser =
  (const AmtLessThan <$> string "<") <|>
  (const AmtGreaterThan <$> string ">") <|>
  (const AmtEqualTo <$> string "=")

intParser :: Parsec String () Int
intParser = do
  mNeg <- option "" (string "-")
  (read . (mNeg ++)) <$> many1 digit

descParser :: Parsec String () DescExpr
descParser = do
  string "description"
  spaces'
  char '"'
  str <- many (noneOf "\"")
  char '"'
  return $ CiContains str

andParser :: Parsec String () FilterExpr
andParser = do
  chainl1 filterExprParser' (spaces' >> (const And <$> string "and") <* spaces')

orParser :: Parsec String () FilterExpr
orParser = do
  chainl1 filterExprParser' (spaces' >> (const Or <$> string "or") <* spaces')
