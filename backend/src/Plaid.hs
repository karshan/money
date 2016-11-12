{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Plaid where

import Money.API (TxnDb, Txn(..))
import Servant.API 
import Servant.Client
import Data.Proxy
import Data.Aeson
import Data.Aeson.TH
import GHC.Generics (Generic)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Bool (bool)
import Web.FormUrlEncoded
import Data.Maybe (mapMaybe)
import Data.IxSet.Typed
import Data.Map (Map)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Time (Day, UTCTime (..))
import qualified Data.Map as Map

parseTime :: String -> String -> Maybe Day
parseTime f = fmap utctDay . parseTimeM True defaultTimeLocale f

data PlaidCredentials =
    PlaidCredentials {
        client_id     :: Text
      , client_secret :: Text
      , access_token  :: Text
    } deriving (Generic)
instance FromJSON PlaidCredentials

data PlaidRequestBody =
    PlaidRequestBody {
        client_id    :: Text
      , secret       :: Text
      , access_token :: Text
      , options      :: Text
    }

instance ToForm PlaidRequestBody where
    toForm PlaidRequestBody{..} =
        toForm
            [ ("client_id" :: Text, client_id)
            , ("secret", secret)
            , ("access_token", access_token)
            , ("options", options)
            ]

data PlaidResponse =
    PlaidResponse {
        accounts     :: [PlaidAccount]
      , transactions :: [PlaidTransaction]
      , access_token :: Text
    } deriving (Generic, Show)
instance FromJSON PlaidResponse
instance ToJSON PlaidResponse

data PlaidAccount =
    PlaidAccount {
        _id              :: Text
      , _item            :: Text
      , _user            :: Text
      , balance          :: PlaidBalance
      , institution_type :: Text
      , meta             :: PlaidAccountMeta
      , subtype          :: Text
      , _type            :: Text
    } deriving (Generic, Show)

data PlaidAccountMeta =
    PlaidAccountMeta {
        limit         :: Maybe Double
      , name          :: Text
      , number        :: Text
      , official_name :: Text
    } deriving (Generic, Show)
instance FromJSON PlaidAccountMeta
instance ToJSON PlaidAccountMeta

data PlaidBalance =
    PlaidBalance {
        available :: Double
      , current   :: Double
    } deriving (Generic, Show)
instance FromJSON PlaidBalance
instance ToJSON PlaidBalance

data PlaidTransaction =
    PlaidTransaction {
        _account    :: Text
      , _id         :: Text
      , amount      :: Double
      , date        :: Text
      , name        :: Text
      , meta        :: PlaidTransactionMeta
      , pending     :: Bool
      , _type       :: PlaidTransactionType
      , category    :: Maybe [Text]
      , category_id :: Maybe Text
      , score       :: PlaidTransactionScore
    } deriving (Generic, Show)

data PlaidTransactionMeta =
    PlaidTransactionMeta {
        location :: Object
    } deriving (Generic, Show)
instance FromJSON PlaidTransactionMeta
instance ToJSON PlaidTransactionMeta

data PlaidTransactionType =
    PlaidTransactionType {
        primary :: Text
    } deriving (Generic, Show)
instance FromJSON PlaidTransactionType
instance ToJSON PlaidTransactionType

data PlaidTransactionScore =
    PlaidTransactionScore {
        location :: Object
      , name     :: Double
    } deriving (Generic, Show)
instance FromJSON PlaidTransactionScore
instance ToJSON PlaidTransactionScore

-- type is a reserved word in haskell so we need to derive the {From,To}JSON
-- instances for PlaidAccount and PlaidTransaction this way.
-- i.e. name the fields _type and choose the json field name for _type as type.
$(deriveJSON defaultOptions{ fieldLabelModifier = \x -> bool x "type" ("_type" == x) } ''PlaidAccount)
$(deriveJSON defaultOptions{ fieldLabelModifier = \x -> bool x "type" ("_type" == x) } ''PlaidTransaction)

type PlaidAPI =
    "connect" :> "get" :> ReqBody '[FormUrlEncoded] PlaidRequestBody
                       :> Post '[JSON] PlaidResponse

plaidGet' :: PlaidRequestBody -> ClientM PlaidResponse
plaidGet' = client (Proxy :: Proxy PlaidAPI)

plaidGet :: PlaidCredentials -> IO (Either ServantError PlaidResponse)
plaidGet PlaidCredentials{..} = do
    mgr <- newManager tlsManagerSettings
    runClientM 
        (plaidGet'
            (PlaidRequestBody
                { client_id = client_id
                , secret = client_secret
                , access_token = access_token
                , options = "" -- {\"pending\": true}"
                }))
        (ClientEnv mgr
            (BaseUrl Https "tartan.plaid.com" 443 ""))

getTxns :: PlaidCredentials -> IO (Either ServantError (TxnDb, Map Text Int))
getTxns creds =
    fmap
        (fmap
            (\plaidResp ->
                let
                    accountMap =
                        Map.fromList
                            (map
                                accMap
                                (accounts plaidResp))
                in
                    (insertList
                        (mapMaybe
                            convPlaidTxn
                            (transactions plaidResp))
                        (empty :: TxnDb), accountMap)))
        (plaidGet creds)
    where
        accMap :: PlaidAccount -> (Text, Int)
        accMap a = (_id (a :: PlaidAccount),
            floor ((current (balance a)) * 100))
        convPlaidTxn :: PlaidTransaction -> Maybe Txn
        convPlaidTxn t = do
            -- TODO don't ignore transaction if date doesn't parse
            dt <- parseTime "%Y-%m-%d" (T.unpack (date (t :: PlaidTransaction)))
            return $
                (Txn
                    (_id (t :: PlaidTransaction))
                    (_account t)
                    (name (t :: PlaidTransaction))
                    dt
                    (floor ((amount (t :: PlaidTransaction)) * 100)))
