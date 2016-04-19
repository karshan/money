{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module API
  ( API
  , MoneyAPI
  , Categorizer
  , RequestLog (..)
  , ResponseLog (..)
  , LogRecord
  , Cred (..)
  , Credential (..)
  , Transaction (..)
  )
  where

import           Servant.API                ((:<|>) (..), (:>), Get,
                                             JSON, Put, Raw, ReqBody)
import           Data.Time          (Day(..))
import           GHC.Generics                 (Generic)
import           Data.Aeson                   (FromJSON, ToJSON)

deriving instance Generic Day
deriving instance FromJSON Day
deriving instance ToJSON Day

data Transaction = Transaction { description :: String
                               , date        :: Day
                               , amount      :: Int
                               } deriving (Show, Read, Eq, Ord, Generic, FromJSON, ToJSON)

                                            
type Categorizer = (String, [String])

data RequestLog = Get String | Post String [(String, String)] deriving (Generic, Show, FromJSON, ToJSON)
data ResponseLog = ResponseLog Int [(String, String)] String [String] deriving (Generic, Show, FromJSON, ToJSON)
type LogRecord = (RequestLog, ResponseLog)

data Cred = Cred { username              :: String
                 , password              :: String
                 , secretQuestionAnswers :: [(String, String)]
                 } deriving (Generic, ToJSON, FromJSON)


data Credential = BankOfAmericaCreds Cred | ChaseCreds Cred deriving (Generic, FromJSON, ToJSON)

type API = MoneyAPI :<|> ("static" :> Raw)

type MoneyAPI =
         "transactions"   :> Get '[JSON] (String, [Transaction])
    :<|> "credentials"    :> Get '[JSON] [(String, String)] -- [(service, username)]
    :<|> "addCredential"  :> ReqBody '[JSON] Credential
                          :> Put '[JSON] ()
    :<|> "logs"           :> Get '[JSON] [([LogRecord], Maybe String)]
    :<|> "categorizers"   :> Get '[JSON] [Categorizer]
    :<|> "addCategorizer" :> ReqBody '[JSON] Categorizer
                          :> Put '[JSON] ()
