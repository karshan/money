{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
module ClientAPI
  ( getTxnDb
  ) where

import Data.Proxy         (Proxy(..))
import Data.ByteString

import Servant.Client

import Money.API (Txn, MoneyAPI)
import Data.Serialize (runGet)
import Data.SafeCopy (safeGet)
import Data.Set (Set)

host :: ClientEnv
host = ClientEnv undefined (BaseUrl Https "money.karshan.me" 443 "")

getTxnDb' :: ClientM (Set Txn)
getTxnDb' = client (Proxy :: Proxy MoneyAPI)

getTxnDb :: IO (Either ServantError (Set Txn))
getTxnDb = runClientM getTxnDb' host
