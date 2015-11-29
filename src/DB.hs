{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module DB
    ( DB
    , DBConfig
    , Error
    , Transactions
    , connect
    , lift
    , runDB
    , getTransactions
    , updateTransactions
    )
    where

import           Control.Lens                ((^?))
import           Control.Monad               (void)
import qualified Control.Monad.Trans         as Trans (lift)
import           Control.Monad.Trans.Either  (EitherT, hoistEither, left)
import           Control.Monad.Trans.Reader  (ReaderT (..))
import qualified Control.Monad.Trans.Reader  as Reader (ask)
import           Data.Aeson                  (FromJSON, Result (..), ToJSON,
                                              Value (..), fromJSON)
import           Data.Aeson.Lens             (key, _Object, _String)
import           Data.Default.Class          (Default (..))
import           Data.HashMap.Strict         (filterWithKey)
import           Data.Text                   (Text)
import qualified Database.Couch.Explicit.Doc as Doc
import           Database.Couch.Types        (Context (..), Db (..), DocId (..),
                                              DocRev (..), Port (..), modifyDoc,
                                              retrieveDoc)
import qualified Database.Couch.Types        as T (Error (..))
import           GHC.Generics                (Generic)
import           Money                       (Transaction)
import           Network.HTTP.Client         (defaultManagerSettings,
                                              newManager)

--
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
--
type DBConfig = Context
type Error = T.Error
newtype DB a = DB { unDB :: ReaderT DBConfig (EitherT Error IO) a } deriving (Functor, Applicative, Monad)

data Transactions = Transactions { transactions :: [Transaction] } deriving (Eq, Ord, Show, Generic)
instance ToJSON Transactions
instance FromJSON Transactions

lift :: IO a -> DB a
lift = DB . Trans.lift . Trans.lift

ask :: DB DBConfig
ask = DB Reader.ask

runDB :: Context -> DB a -> EitherT Error IO a
runDB ctx a = runReaderT (unDB a) ctx


runDoc :: FromJSON b => IO (Either Error (Value, a)) -> DB (Maybe (b, Text))
runDoc d = do
    val <- fmap fst (DB . Trans.lift . hoistEither =<< lift d)
    return $ parseDoc val

resultToMaybe :: Result a -> Maybe a
resultToMaybe (Success a) = Just a
resultToMaybe (Error _)   = Nothing

parseDoc :: (FromJSON a) => Value -> Maybe (a, Text)
parseDoc val = do
    object <- val ^? _Object
    revision <- val ^? key "_rev" . _String
    res <- resultToMaybe $ fromJSON $ Object $ filterWithKey (\k _ -> k /= "_id" && k /= "_rev") object
    return (res, revision)

connect :: IO DBConfig
connect = do
    mgr <- newManager defaultManagerSettings
    return $ Context mgr "localhost" (Port 5984) Nothing def (Just (Db "money"))

getTransactions :: DB (Transactions, Text)
getTransactions = do
    ctx <- ask
    v <- runDoc $ Doc.get retrieveDoc (DocId "transactions") Nothing ctx
    maybe (DB $ Trans.lift $ left (T.ParseFail "miasma")) return v

updateTransactions :: ([Transaction] -> [Transaction]) -> DB ()
updateTransactions f = do
    ctx <- ask
    (Transactions ts, rev) <- getTransactions
    (void :: DB (Maybe (Value, Text)) -> DB ()) $ runDoc $ Doc.put modifyDoc (DocId "transactions") (Just (DocRev rev)) (Transactions $ f ts) ctx
