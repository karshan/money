module Util where

import List exposing (map2)
import Sha
import String exposing (contains, toLower)

zip : List a -> List b -> List (a, b)
zip = map2 (,)

ciContains : String -> String -> Bool
ciContains a b = toLower a `contains` toLower b

sha256 : String -> String
sha256 s = Sha.digest "hex" <| Sha.update s "utf8" <| Sha.createHash "sha256"
