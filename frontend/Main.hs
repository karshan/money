{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
import Control.Monad.Trans.Either
import Control.Monad.IO.Class

import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Proxy         (Proxy(..))
import qualified Data.Text as T
import           Data.Time          (Day)

import           GHC.Generics       (Generic)

import Reflex
import Reflex.Dom
import Reflex.Dom.Xhr

import Servant.Reflex
import Servant.API

import GHCJS.Marshal
import GHCJS.Prim

data Transaction = Transaction { description :: String
                               , date        :: String
                               , amount      :: Int
                               } deriving (Show, Read, Eq, Ord, Generic, FromJSON, ToJSON, ToJSVal, FromJSVal)

type API =
         "transactions"   :> Get '[JSON] String

getTransactions :: MonadWidget t m => Event t () -> m (Event t (Maybe String, XhrResponse))
getTransactions = client (Proxy :: Proxy API) (Proxy :: Proxy m) (constDyn host)
  where
    host = BaseUrl Https "karshan.me" 443 ""

main = do
    mainWidget $ el "div" $ do
    pb <- getPostBuild
    let req = xhrRequest "GET" "/transactions" def
    responseEvent <- performRequestAsync (const req <$> pb)
    responseDynamic <- holdDyn Nothing (_xhrResponse_responseText <$> responseEvent)
    dynText =<< (mapDyn show responseDynamic)
