module Util where

import List exposing (map2)
import String exposing (contains, toLower)

zip : List a -> List b -> List (a, b)
zip = map2 (,)

ciContains : String -> String -> Bool
ciContains a b = toLower a `contains` toLower b
