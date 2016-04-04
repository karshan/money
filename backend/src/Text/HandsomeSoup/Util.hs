module Text.HandsomeSoup.Util
    ( css
    , css'
    , cssSingle
    , cssSingle'
    ) where

import           Data.ByteString.Lazy      (toStrict)
import qualified Data.ByteString.Lazy      as LBS (ByteString)
import           Data.ByteString.UTF8      (toString)
import           Data.Maybe                (listToMaybe)
import           Data.Tree.NTree.TypeDefs  (NTree)
import qualified Text.HandsomeSoup         as HS (css)
import           Text.XML.HXT.Core         (LA, hread, runLA, (>>>))
import           Text.XML.HXT.DOM.TypeDefs (XNode)

css :: String -> LA (NTree XNode) a -> String -> [a]
css query f = runLA (hread >>> HS.css query >>> f)

css' :: String -> LA (NTree XNode) a -> LBS.ByteString -> [a]
css' query f = css query f . toString . toStrict

cssSingle :: String -> LA (NTree XNode) a -> String -> Maybe a
cssSingle query f = listToMaybe . css query f

cssSingle' :: String -> LA (NTree XNode) a -> LBS.ByteString -> Maybe a
cssSingle' query f = listToMaybe . css' query f
