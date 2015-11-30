{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-unused-binds  #-}
module DB
    ( DBConfig
    , Error
    , Transactions
    , connect
    , getTransactions
    , updateTransactions
    )
    where

import           Control.Lens                ((^?))
import           Control.Monad.Except        (ExceptT, MonadError, runExceptT,
                                              throwError)
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Reader        (MonadReader, ReaderT, ask,
                                              runReaderT)
import           Data.Aeson                  (FromJSON, ToJSON, Value (..),
                                              fromJSON)
import           Data.Aeson.Lens             (key, _Object, _String)
import           Data.Aeson.Util             (resultToEither)
import           Data.Bool                   (bool)
import           Data.Default.Class          (Default (..))
import           Data.HashMap.Strict         (filterWithKey)
import           Data.Maybe.Util             (maybeToEither)
import           Data.Text                   (Text)
import qualified Database.Couch.Explicit.Doc as Couch.Doc
import           Database.Couch.Response     (asBool)
import           Database.Couch.Types        (Context (..), Db (..), DocId (..),
                                              DocRev (..), Port (..), modifyDoc,
                                              retrieveDoc)
import qualified Database.Couch.Types        as Couch (Error (..))
import           GHC.Generics                (Generic)
import           Money                       (Transaction)
import           Network.HTTP.Client         (defaultManagerSettings,
                                              newManager)

-- Mainly using CouchDB for atomic operations
-- store a single document in database money named transactions
-- of the form:
-- {
--   "transactions": [
--     { "description": "Chipotle", ...},
--     ...
--   ],
--   // Fields in every couchdb Doc
--   "_id": "transactions"
--   "_rev": "2_akjnkanc..."
-- }
-- so now we can grab the list of transactions and update the list
-- atomically (basically we will fail if someone did an update in
-- the middle of ours)

type DBConfig = Context

data Error = CouchError Couch.Error | MyError String deriving (Show)

newtype DB a = DB { unDB :: ReaderT DBConfig (ExceptT Error IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader DBConfig, MonadError Error)

data Transactions = Transactions { transactions :: [Transaction] } deriving (Generic)
instance ToJSON Transactions
instance FromJSON Transactions

connect :: IO DBConfig
connect = do
    mgr <- newManager defaultManagerSettings
    return $ Context mgr "localhost" (Port 5984) Nothing def (Just (Db "money"))

runDB :: DBConfig -> DB a -> IO (Either Error a)
runDB ctx a = runExceptT (runReaderT (unDB a) ctx)

parseDoc :: (FromJSON a) => Value -> Either String (a, Text)
parseDoc val = do
    object <- maybeToEither "value not an object" $ val ^? _Object
    revision <- maybeToEither "no _rev key" $ val ^? key "_rev" . _String
    res <- resultToEither $ fromJSON $ Object $ filterWithKey (\k _ -> k /= "_id" && k /= "_rev") object
    return (res, revision)

runCouch :: (MonadIO m, MonadError Error m) => m (Either Couch.Error (a, b)) -> m a
runCouch d = either (throwError . CouchError) (return . fst) =<< d

-- getTransactions :: (MonadIO m, MonadError Error m, MonadReader DBConfig m) => m (Transactions, Text)
getTransactions :: DB (Transactions, Text)
getTransactions = do
    ctx <- ask
    v <- runCouch $ Couch.Doc.get retrieveDoc (DocId "transactions") Nothing ctx
    either (throwError . MyError) return $ parseDoc v

updateTransactions :: ([Transaction] -> [Transaction]) -> DB ()
updateTransactions f = do
    ctx <- ask
    (Transactions ts, rev) <- getTransactions
    v <- runCouch $ asBool <$> Couch.Doc.put modifyDoc (DocId "transactions") (Just (DocRev rev)) (Transactions $ f ts) ctx
    bool (throwError $ MyError "Doc.put returned false") (return ()) v
