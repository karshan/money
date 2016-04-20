{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Scrapers.Browser.Log
    ( RequestLog (..)
    , ResponseLog (..)
    , LogRecord
    , mkResponseLog
    , mkPost
    ) where

import           Control.Lens                 (both, (%~), (^.))
import           Data.ByteString              (ByteString)
import           Data.ByteString.Lazy         (toStrict)
import qualified Data.ByteString.Lazy         as LBS (ByteString)
import           Data.ByteString.UTF8         (toString)
import           Data.CaseInsensitive         (original)
import           Network.HTTP.Client.Internal (expose)
import           Network.Wreq                 (Response, responseBody,
                                               responseCookieJar,
                                               responseHeaders, responseStatus,
                                               statusCode)
import           Money.API (RequestLog (..), ResponseLog (..), LogRecord)

mkResponseLog :: Response LBS.ByteString -> ResponseLog
mkResponseLog r = ResponseLog (r ^. responseStatus ^. statusCode)
                              (map (\(a, b) -> (toString $ original a, toString b)) $ r ^. responseHeaders)
                              (toString $ toStrict $ r ^. responseBody)
                              (map show $ expose $ r ^. responseCookieJar)

mkPost :: String -> [(ByteString, ByteString)] -> RequestLog
mkPost a = Post a . map (both %~ toString)
