module Money
    (
      Txn(..)
    , month
    )
    where

import Data.Time.Calendar (toGregorian)
import Money.API (Txn (..))

month :: Txn -> Int
month = (\(_,m,_) -> m) . toGregorian . date
