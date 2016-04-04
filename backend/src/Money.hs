module Money
    (
      Transaction(..)
    , month
    )
    where

import           Data.Time.Calendar (toGregorian)
import           Money.API          (Transaction (..))

month :: Transaction -> Int
month = (\(_,m,_) -> m) . toGregorian . date
