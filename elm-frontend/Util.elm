module Util where

import List exposing (map2)
import Sha
import String exposing (contains, toLower)

type Either a b =
    Left a
  | Right b

either : (a -> c) -> (b -> c) -> Either a b -> c
either f g e =
  case e of
    Left a -> f a
    Right b -> g b

zip : List a -> List b -> List (a, b)
zip = map2 (,)

ciContains : String -> String -> Bool
ciContains a b = toLower a `contains` toLower b

sha256 : String -> String
sha256 s = Sha.digest "hex" <| Sha.update s "utf8" <| Sha.createHash "sha256"
