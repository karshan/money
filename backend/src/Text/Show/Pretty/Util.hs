module Text.Show.Pretty.Util
    ( print
    ) where

import           Prelude          hiding (print)
import           Text.Show.Pretty (ppShow)

print :: (Show a) => a -> IO ()
print = putStrLn . ppShow
