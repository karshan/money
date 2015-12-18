module Filter where

import Combine       exposing (Parser, Result (..), many, sepBy, between, end
                              , parse, choice, string, regex, andThen, maybe
                              , rec, chainl, parens, succeed)
import Combine.Infix exposing ((<$>), (<$), (<*), (*>), (<|>))
import Combine.Num   exposing (digit, int)
import List          exposing (foldr, map2, member)
import Maybe         exposing (withDefault)
import Model         exposing (Transaction)
import Util          exposing (ciContains)

type NumOp = Lt | Gt | NumEq
type StringOp = CiContains
type BinaryOp = And | Or

type Expr =
    Not Expr
  | BinaryOp BinaryOp Expr Expr
  | StringOp StringOp String
  | NumOp NumOp Int
  | TagOp String

(>>=) = andThen

eval : Expr -> Transaction -> Bool
eval e t =
    case e of
      (Not e)                               -> not <| eval e t
      (BinaryOp And e1 e2)                  -> eval e1 t && eval e2 t
      (BinaryOp Or e1 e2)                   -> eval e1 t || eval e2 t
      (StringOp CiContains s)               -> s `ciContains` t.description -- GG contains is totally backwards
      (NumOp op n)                          -> evalNumOp op (-1 * t.amount) n -- this -1 is because we display -1 * amounts...
      (TagOp s)                             -> s `member` t.tags

evalNumOp : NumOp -> Int -> Int -> Bool
evalNumOp op amt n =
    case op of
        Lt    -> amt < n
        Gt    -> amt > n
        NumEq -> amt == n

ws : Parser String
ws = regex "[ \t\r\n]*"

not_ : Parser Expr
not_ = Not <$> (string "not" *> expr)

binOpAnd : Parser Expr
binOpAnd = (string "and" *> ws *> expr) >>= (\e1 -> BinaryOp And e1 <$> (ws *> expr))

binOpOr : Parser Expr
binOpOr = (string "or"  *> ws *> expr) >>= (\e1 -> BinaryOp Or  e1 <$> (ws *> expr))

stringLit : Parser String
stringLit = between (string "\"") (string "\"") (regex "[^\"]*")

stringOp : Parser Expr
stringOp =
    StringOp CiContains <$>
        (string "desc" *> between ws ws (string "<>") *> stringLit)

numOp_ : Parser NumOp
numOp_ = choice [ Lt <$ string "<", Gt <$ string ">", NumEq <$ string "=" ]

numOp : Parser Expr
numOp =
    (string "amt" *> between ws ws numOp_) >>= (\op -> NumOp op <$> currency)

tagOp : Parser Expr
tagOp =
    (string "tag" *> between ws ws (string "<>")) >>= (\op -> TagOp <$> stringLit)

expr : Parser Expr
expr = ws *> rec (\() -> stringOp <|> numOp <|> tagOp <|> parens expr <|> binOpAnd <|> binOpOr <|> not_)

currency : Parser Int
currency =
    let decimalPart = (foldr (+) 0 << map2 (*) [10, 1]) <$> (string "." *> (many digit))
    in int >>= (\n -> ((+) (n * 100)) <$> (withDefault 0 <$> maybe decimalPart))

doFilter : String -> Transaction -> Bool
doFilter s t =
    case parse (expr <* end) s of
      (Done e, _) -> eval e t
      (Fail _, _) -> True

parseSuccess : String -> Bool
parseSuccess s =
    case parse (expr <* end) s of
      (Done _, _) -> True
      (Fail _, _) -> False
