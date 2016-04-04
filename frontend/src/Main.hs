{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
import Control.Monad.Trans.Either
import Control.Monad.IO.Class

import Data.Proxy         (Proxy(..))
import Data.Time          (Day(..))
import Reflex.Dom

import Servant.Client
import Servant.API

import GHCJS.Marshal
import GHCJS.Prim

import Money.API

-- TODO: these insances are fucked
instance ToJSVal Integer where
  toJSVal = toJSVal . (fromInteger :: Integer -> Int)

instance FromJSVal Integer where
  fromJSVal = (fmap (fmap toInteger)) . (fromJSVal :: JSVal -> IO (Maybe Int))

deriving instance ToJSVal Day
deriving instance FromJSVal Day
deriving instance ToJSVal Transaction
deriving instance FromJSVal Transaction
deriving instance FromJSVal ResponseLog
deriving instance FromJSVal RequestLog
deriving instance ToJSVal Cred
deriving instance ToJSVal Credential

getTransactions :: EitherT ServantError IO (String, [Transaction])
getCreds :: EitherT ServantError IO [(String, String)]
addCred :: Credential -> EitherT ServantError IO ()
getLogs :: EitherT ServantError IO [([LogRecord], Maybe String)]
getCats :: EitherT ServantError IO [Categorizer]
addCat :: (String, [String]) -> EitherT ServantError IO ()
getTransactions :<|> getCreds :<|> addCred :<|> getLogs :<|> getCats :<|> addCat = client (Proxy :: Proxy MoneyAPI) (Just host)
  where
    host = BaseUrl Https "money.karshan.me" 443

main :: IO ()
main = do
    mainWidget $ el "div" $ do
    ts <- liftIO $ runEitherT getTransactions
    text (either (const "error") show ts)
