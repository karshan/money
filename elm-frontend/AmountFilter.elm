module AmountFilter where

import Combine       exposing (Parser, Result (..), many, sepBy, between, end, parse, choice, string, regex, andThen, maybe)
import Combine.Infix exposing ((<$>), (<$), (<*), (*>))
import Combine.Num   exposing (int, digit)
import List          exposing (foldr, map2)
import Maybe         exposing (withDefault)

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

eval : List (Op, Int) -> Int -> Bool
eval e n =
    case e of
      [] -> True
      ((LT, x)::xs) -> n < x && eval xs n
      ((GT, x)::xs) -> n > x && eval xs n
      ((EQ, x)::xs) -> n == x && eval xs n

ws : Parser String
ws = regex "[ \t\r\n]*"

expr : Parser (List (Op, Int))
expr = between ws ws (sepBy ws basicExpr)

basicExpr : Parser (Op, Int)
basicExpr = op >>= (\op' -> ws *> ((,) op' <$> currency))

currency : Parser Int
currency = int >>= (\n -> ((+) (n * 100)) <$> (withDefault 0 <$> maybe decimal))

decimal : Parser Int
decimal = (foldr (+) 0 << map2 (*) [10, 1]) <$> (string "." *> (many digit))

op : Parser Op
op = choice [ LT <$ string "<", GT <$ string ">", EQ <$ string "=" ]
