{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Scrapers
    ( Cred(..)
    , Credential(..)
    ) where

import Data.Aeson   (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- encode $ BankOfAmericaCreds $ Cred "hi" "there" [("a","b")] =
-- {
--   "tag": "BankOfAmericaCreds",
--   "contents": {
--     "username":"hi",
--     "password":"there",
--     "secretQuestionAnswers": [["a","b"]]
--   }
-- }
data Credential = BankOfAmericaCreds Cred | ChaseCreds Cred deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data Cred = Cred { username :: String
                 , password :: String
                 , secretQuestionAnswers :: [(String, String)]
                 } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)
