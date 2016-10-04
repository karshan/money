{-# LANGUAGE LambdaCase #-}
module Util (
    htmlEntities
) where

htmlEntities :: String -> String
htmlEntities =
    concatMap (\case
        '<' -> "&lt;"
        '>' -> "&gt;"
        '\'' -> "&#34;"
        '"' -> "&quot;"
        x -> [x])
