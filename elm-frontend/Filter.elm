module Filter where

import Combine       exposing (Parser, Result (..), many, sepBy, between, end
                              , parse, choice, string, regex, andThen, maybe
                              , rec, chainl, parens, succeed)
import Combine.Infix exposing ((<$>), (<$), (<*), (*>), (<|>), (<?>))
import Combine.Num   exposing (digit, int)
import List          exposing (foldr, map2, member)
import Maybe         exposing (withDefault)
import Model         exposing (Transaction, Expr(..), NumOp(..), StringOp(..), BinaryOp(..))
import String        exposing (fromChar, foldl)
import Util          exposing (Either (..), ciContains)

(>>=) = andThen

parseFilter : String -> Either String Expr
parseFilter s =
    case parse (expr <* end) s of
        (Done e, _) -> Right e
        _ -> Left <| parseDebug s

doFilter : String -> Transaction -> Bool
doFilter s t =
    case parse (expr <* end) s of
      (Done e, _) -> eval e t
      (Fail _, _) -> True

eval : Expr -> Transaction -> Bool
eval e t =
    case e of
      (Not e)                               -> not <| eval e t
      (BinaryOp And e1 e2)                  -> eval e1 t && eval e2 t
      (BinaryOp Or e1 e2)                   -> eval e1 t || eval e2 t
      (StringOp CiContains s)               -> s `ciContains` t.description -- GG contains is totally backwards
      (NumOp op n)                          -> evalNumOp op (-1 * t.amount) n -- this -1 is because we display -1 * amounts...

evalNumOp : NumOp -> Int -> Int -> Bool
evalNumOp op amt n =
    case op of
        Lt    -> amt < n
        Gt    -> amt > n
        NumEq -> amt == n

ws : Parser String
ws = regex "[ \t\r\n]*" <?> "whitespace"

not_ : Parser Expr
not_ = (Not <$> (string "not" *> expr)) <?> "notE"

binOpAnd : Parser Expr
binOpAnd = ((string "and" *> ws *> expr) >>= (\e1 -> BinaryOp And e1 <$> (ws *> expr))) <?> "binOpAnd"

binOpOr : Parser Expr
binOpOr = ((string "or"  *> ws *> expr) >>= (\e1 -> BinaryOp Or  e1 <$> (ws *> expr))) <?> "binOpOr"

unescapeQuotes : String -> String
unescapeQuotes =
    let f c (acc, s) =
        if s == False then
            if c == '\\' then
                (acc, True)
            else
                (acc ++ fromChar c, False)
        else
            if c == '"' then
                (acc ++ "\"", False)
            else
                (acc ++ "\\" ++ fromChar c, False)
    in fst << foldl f ("", False)

stringLit : Parser String
stringLit = (string "\"" *> (unescapeQuotes <$> regex "(\\\\\"|[^\"\n])*") <* string "\"") <?> "stringLit"

desc : Parser Expr
desc =
    (StringOp CiContains <$>
        (string "desc" *> between ws ws (string "<>") *> stringLit)) <?> "descFilter"

numOp : Parser NumOp
numOp = choice [ Lt <$ string "<", Gt <$ string ">", NumEq <$ string "=" ] <?> "numOp"

amt : Parser Expr
amt =
    (string "amt" *> between ws ws numOp) >>= (\op -> NumOp op <$> currency) <?> "amtFilter"

expr : Parser Expr
expr = ws *> rec (\() -> desc <|> amt <|> parens expr <|> binOpAnd <|> binOpOr <|> not_)

currency : Parser Int
currency =
    let decimalPart = (foldr (+) 0 << map2 (*) [10, 1]) <$> (string "." *> (many digit))
    in int >>= (\n -> ((+) (n * 100)) <$> (withDefault 0 <$> maybe decimalPart))

parseSuccess : String -> Bool
parseSuccess s =
    case parse (expr <* end) s of
      (Done _, _) -> True
      (Fail _, _) -> False

parseDebug : String -> String
parseDebug s = toString <| parse (expr <* end) s
