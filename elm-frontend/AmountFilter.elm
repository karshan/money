module AmountFilter where

import Combine       exposing (Parser, Result (..), rec, chainl, parens, between, end, parse, map, choice, string, regex, andThen)
import Combine.Char  exposing (char)
import Combine.Infix exposing ((<$>), (<$), (<*>), (<*), (*>), (<|>))
import Combine.Num   exposing (int)

type Expr =
      Op Op Int
    | And Expr Expr
    | Or Expr Expr

type Op = LT | GT | EQ

(>>=) = andThen

doAmountFilter : String -> Int -> Bool
doAmountFilter s n =
    case parse (expr <* end) s of
      (Done e, _) -> eval e n
      (Fail _, _) -> True

parseSuccess : String -> Bool
parseSuccess s =
    case parse (expr <* end) s of
      (Done _, _) -> True
      (Fail _, _) -> False

eval : Expr -> Int -> Bool
eval e n =
    case e of
      (Op LT x) -> n < x
      (Op GT x) -> n > x
      (Op EQ x) -> n == x
      (And e1 e2) -> eval e1 n && eval e2 n
      (Or e1 e2) -> eval e1 n || eval e2 n

ws : Parser String
ws = regex "[ \t\r\n]*"

and : Parser (Expr -> Expr -> Expr)
and = And <$ string "&"

or : Parser (Expr -> Expr -> Expr)
or = Or <$ string "|"

expr : Parser Expr
expr = rec <| \() -> term `chainl` or

term : Parser Expr
term = rec <| \() -> factor `chainl` and

factor : Parser Expr
factor = rec <| \() -> between ws ws (parens expr <|> basicExpr)

basicExpr : Parser Expr
basicExpr = op >>= (\op' -> ws *> (Op op' <$> int))

op : Parser Op
op = choice [ LT <$ string "<", GT <$ string ">", EQ <$ string "=" ]
