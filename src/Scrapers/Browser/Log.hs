{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Scrapers.Browser.Log
    ( RequestLog (..)
    , ResponseLog (..)
    , LogRecord
    , mkResponseLog
    , mkPost
    ) where

import           Control.Lens                 (both, (%~), (^.))
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.ByteString              (ByteString)
import           Data.ByteString.Lazy         (toStrict)
import qualified Data.ByteString.Lazy         as LBS (ByteString)
import           Data.ByteString.UTF8         (toString)
import           Data.CaseInsensitive         (original)
import           GHC.Generics                 (Generic)
import           Network.HTTP.Client.Internal (expose)
import           Network.Wreq                 (Response, responseBody,
                                               responseCookieJar,
                                               responseHeaders, responseStatus,
                                               statusCode)

data RequestLog = Get String | Post String [(String, String)] deriving (Generic, Show, FromJSON, ToJSON)
data ResponseLog = ResponseLog Int [(String, String)] String [String] deriving (Generic, Show, FromJSON, ToJSON)
type LogRecord = (RequestLog, ResponseLog)

mkResponseLog :: Response LBS.ByteString -> ResponseLog
mkResponseLog r = ResponseLog (r ^. responseStatus ^. statusCode)
                              (map (\(a, b) -> (toString $ original a, toString b)) $ r ^. responseHeaders)
                              (toString $ toStrict $ r ^. responseBody)
                              (map show $ expose $ r ^. responseCookieJar)

mkPost :: String -> [(ByteString, ByteString)] -> RequestLog
mkPost a = Post a . map (both %~ toString)
