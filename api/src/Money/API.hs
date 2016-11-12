{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric         #-}
module Money.API
  ( API
  , MoneyAPI
  , Txn (..)
  , TxnDb
  , AccountId(..)
  , Desc(..)
  , Amt(..)
  , Date(..)
  )
  where

import Servant.API ((:<|>) (..), (:>), Get,
                    JSON, Raw)
import Data.ByteString (ByteString)
import Data.Time (Day(..))
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Proxy (Proxy(..))
import Data.IxSet.Typed
import Data.SafeCopy          (base, deriveSafeCopy)
import Data.Text              (Text)
import Data.Set               (Set)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data Txn =
    Txn {
        _id         :: Text
      , accountId   :: Text
      , description :: Text
      , date        :: Day
      , amount      :: Int
    } deriving (Show, Eq, Ord, Data, Typeable, Generic)
instance FromJSON Txn
instance ToJSON Txn

type TxnIxs = '[AccountId, Txn, Desc, Amt, Date]
type TxnDb = IxSet TxnIxs Txn

newtype AccountId = AccountId Text deriving (Show, Eq, Ord)
newtype Desc = Desc Text deriving (Show, Eq, Ord)
newtype Amt = Amt Int deriving (Show, Eq, Ord)
newtype Date = Date Day deriving (Show, Eq, Ord)

instance Indexable TxnIxs Txn where
    indices = ixList
                (ixFun getAccountId)
                (ixGen (Proxy :: Proxy Txn))
                (ixFun getDesc)
                (ixFun getAmt)
                (ixFun getDate)

getDesc :: Txn -> [Desc]
getDesc t = [Desc (description t)]

getAmt :: Txn -> [Amt]
getAmt t = [Amt (amount t)]

getDate :: Txn -> [Date]
getDate t = [Date (date t)]

getAccountId :: Txn -> [AccountId]
getAccountId t = [AccountId (accountId t)]

type API = MoneyAPI :<|> ("static" :> Raw)

type MoneyAPI =
         "transactions"   :> Get '[JSON] (Set Txn)

$(deriveSafeCopy 0 'base ''Txn)
