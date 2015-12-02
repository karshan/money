{-# LANGUAGE LambdaCase #-}
module Scrapers
    ( Cred (..)
    , Credential (..)
    , getAllTransactions
    ) where

import           Money                  (Transaction)
import qualified Scrapers.BankOfAmerica as BankOfAmerica (getAllTransactions)
import           Scrapers.Common        (Cred (..), Credential (..))

getAllTransactions :: [Credential] -> IO [Transaction]
getAllTransactions creds = concat <$> mapM (\case
    BankOfAmericaCreds c -> BankOfAmerica.getAllTransactions c
    _                    -> return []) creds
