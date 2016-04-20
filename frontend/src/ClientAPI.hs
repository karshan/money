{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
module ClientAPI
  ( getTransactions
  , getCreds
  , addCred
  , getLogs
  , getCats
  , addCat
  , evaluate
  ) where

import Control.Monad.Trans.Either
import Control.Monad.IO.Class

import qualified Data.Aeson.Types as AE (Value, Result(Error))
import Data.Aeson.Types
import Data.Default.Class
import Data.Proxy         (Proxy(..))
import Data.Time          (Day(..))
import Reflex.Dom
import Reflex.Dom.Widget.Input

import Servant.Client
import Servant.API

import GHCJS.Marshal
import GHCJS.Prim
import GHCJS.Foreign.Internal

import Money.API

resultToMaybe :: Result a -> Maybe a
resultToMaybe (AE.Error _) = Nothing
resultToMaybe (Success a) = Just a

fromJSVal_aeson :: FromJSON a => JSVal -> IO (Maybe a)
fromJSVal_aeson = fmap (\a -> (resultToMaybe . fromJSON) =<< a) . (fromJSVal :: JSVal -> IO (Maybe AE.Value))

-- We can't derive ToJSVal and FromJSVal because that results in a different instance than these.
-- specifically for sum types the serialization for the deriving ToJSVal instance is different
-- from the Aeson.ToJSON instance
instance ToJSVal Integer where
  toJSVal = toJSVal_aeson
instance FromJSVal Integer where
  fromJSVal = fromJSVal_aeson

instance ToJSVal Day where
  toJSVal = toJSVal_aeson
instance FromJSVal Day where
  fromJSVal = fromJSVal_aeson

instance ToJSVal Transaction where
  toJSVal = toJSVal_aeson
instance FromJSVal Transaction where
  fromJSVal = fromJSVal_aeson

instance ToJSVal ResponseLog where
  toJSVal = toJSVal_aeson
instance FromJSVal ResponseLog where
  fromJSVal = fromJSVal_aeson

instance ToJSVal RequestLog where
  toJSVal = toJSVal_aeson
instance FromJSVal RequestLog where
  fromJSVal = fromJSVal_aeson

instance ToJSVal Cred where
  toJSVal = toJSVal_aeson
instance FromJSVal Cred where
  fromJSVal = fromJSVal_aeson

instance ToJSVal Credential where
  toJSVal = toJSVal_aeson
instance FromJSVal Credential where
  fromJSVal = fromJSVal_aeson

getTransactions :: EitherT ServantError IO (String, [Transaction])
getCreds :: EitherT ServantError IO [(String, String)]
addCred :: Credential -> EitherT ServantError IO ()
getLogs :: EitherT ServantError IO [([LogRecord], Maybe String)]
getCats :: EitherT ServantError IO [Categorizer]
addCat :: (String, [String]) -> EitherT ServantError IO ()
getTransactions :<|> getCreds :<|> addCred :<|> getLogs :<|> getCats :<|> addCat = client (Proxy :: Proxy MoneyAPI) (Just host)
  where
    host = BaseUrl Https "money.karshan.me" 443

evaluate :: (MonadWidget t m, Reflex t) => (a -> IO b) -> Event t a -> m (Event t b)
evaluate f actions = performEvent $ fmap (liftIO . f) actions
