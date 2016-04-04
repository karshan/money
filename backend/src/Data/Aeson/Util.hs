module Data.Aeson.Util
    ( resultToEither
    ) where

import           Data.Aeson (Result (..))

resultToEither :: Result a -> Either String a
resultToEither (Success a) = Right a
resultToEither (Error s)   = Left s
