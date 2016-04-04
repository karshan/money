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
import           Money.API          (Transaction (..))

month :: Transaction -> Int
month = (\(_,m,_) -> m) . toGregorian . date
