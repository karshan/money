{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Money
    (
      Transaction(..)
    , month
    )
    where

import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Time          (Day)
import           Data.Time.Calendar (toGregorian)
import           GHC.Generics       (Generic)

data Transaction = Transaction { description :: String
                               , date        :: Day
                               , amount      :: Int
                               } deriving (Show, Read, Eq, Ord, Generic, FromJSON, ToJSON)

month :: Transaction -> Int
month = (\(_,m,_) -> m) . toGregorian . date
