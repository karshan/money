{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-unused-binds  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module DB
    ( DB
    , DB'
    , DBContext
    , runDB
    , openDB
    , getTxnDb
    , mergeTxns
    )
    where

import Control.Lens           (view, (^.))
import Control.Lens.TH        (makeLenses)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State    (get, put)
import Control.Monad.Reader   (MonadReader, ReaderT, ask, runReaderT)
import Data.Acid              (AcidState, Query, makeAcidic, Update,
                               openLocalStateFrom, query, update)
import Data.SafeCopy          (base, deriveSafeCopy)
import Money.API              (TxnDb)
import Data.IxSet.Typed       (empty)
import qualified Data.IxSet.Typed as IxSet (union, size)
import Data.Text              (Text)
import Data.Map.Strict        (Map)
import qualified Data.Map.Strict as Map

type DBContext = AcidState Database
data Database = Database {
    _txnDb :: TxnDb
  , _bals :: Map Text Int
  }

makeLenses ''Database

$(deriveSafeCopy 0 'base ''Database)

getTxnDb_ :: Query Database TxnDb
getTxnDb_ = view txnDb <$> ask

mergeTxns_ :: (TxnDb, Map Text Int) -> Update Database Int
mergeTxns_ (newTxns, newBals) = do
    db <- get
    let (numUpdated, updated) = mergeTxnsPure newTxns (db ^. txnDb)
    let updatedBals = mergeBals newBals (db ^. bals)
    put (db { _txnDb = updated })
    return numUpdated

mergeTxnsPure :: TxnDb -> TxnDb -> (Int, TxnDb)
mergeTxnsPure new old =
    let u = IxSet.union new old
    in (IxSet.size u - IxSet.size old, u)

mergeBals :: Map Text Int -> Map Text Int -> Map Text Int
mergeBals = Map.unionWith (\new _ -> new)
    
$(makeAcidic ''Database [ 'getTxnDb_
                        , 'mergeTxns_
                        ])

type DB m = (MonadReader DBContext m, MonadIO m)
type DB' = ReaderT DBContext IO

runDB :: DBContext -> DB' a -> IO a
runDB db a = runReaderT a db

openDB :: FilePath -> IO DBContext
openDB fp = openLocalStateFrom fp (Database (empty :: TxnDb) Map.empty)

update' a = liftIO . update a
query' a = liftIO . query a

--TODO generate with TemplateHaskell
getTxnDb :: DB m => m TxnDb
getTxnDb = (`query'` GetTxnDb_) =<< ask

mergeTxns :: DB m => (TxnDb, Map Text Int) -> m Int
mergeTxns new = (`update'` MergeTxns_ new) =<< ask
