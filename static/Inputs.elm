module Inputs
       ( transactionClicks
       ) where

import Model (Transaction)
import Graphics.Input (Input, input)

transactionClicks : Input (Maybe Transaction)
transactionClicks = input Nothing
