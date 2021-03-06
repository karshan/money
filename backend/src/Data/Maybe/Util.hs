module Data.Maybe.Util
    ( maybeToEither
    ) where

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _ (Just a) = Right a
maybeToEither e Nothing  = Left e
