module ManageDB
    ( ManageDB.init
    , dropDatabase
    , addTransaction
    , getTransactions
    , addTransactions
    , deleteTransaction
    , updateTransaction
    , updateTransactions
    )
    where

import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Database.CouchDB (CouchMonad, DB, Doc, Rev, runCouchDB', createDB, dropDB, newNamedDoc, db, doc, getDoc, updateDoc)
import Data.List (delete)
import Money (Transaction)
import Text.JSON (JSON(..), JSValue(..), Result(..), toJSObject, fromJSObject)

{-
 - Mainly using CouchDB for atomic operations
 - store a single document in database money named transactions
 - of the form:
 - {
 -   "transactions": [
 -     { "description": "Chipotle", ...},
 -     ...
 -   ]
 - }
 - so now we can grab the list of transactions and update the list
 - atomically (basically we will fail if someone did an update in
 - the middle of ours)
-}

dbName :: String
dbName = "money"

-- TODO better name
thedb :: DB
thedb = db dbName

docName :: String
docName = "transactions"

-- TODO better name
theDoc :: Doc
theDoc = doc docName

-- TODO maybe there is a cleaner way to do this with newtype ?
-- This exists solely because couchdb documents must be json objects
data Transactions = Transactions [Transaction] deriving (Show, Eq)
instance JSON Transactions where
    readJSON (JSObject o) = fmap Transactions $ jslookup "transactions" $ fromJSObject o
            where
                jslookup k l = maybe (Error $ "missing key: " ++ k) readJSON (lookup k l)
    readJSON _ = Error "not an object"
    showJSON (Transactions ts) = showJSON $ toJSObject [("transactions", ts)]


-- TODO reimplement this as a Monad with runDB so that it
-- is unnecessary to call init

-- If this fails CouchDB is down or the db already exists
-- either way we don't care. (I guess we kinda care if
-- couchDB is down.... TODO)
init :: IO ()
init = cdb >> cdoc >> return ()
    where
        cdb = runCouchDB' (createDB dbName) `catch` (const (return ()) :: SomeException -> IO ())
        cdoc = runCouchDB' $ newNamedDoc thedb theDoc (Transactions [])

-- TODO handle exceptions/failure
dropDatabase :: IO ()
dropDatabase = void $ runCouchDB' $ dropDB dbName

-- TODO use MaybeT IO a instead of IO (Maybe a)
updateTransactions :: ([Transaction] -> [Transaction]) -> IO (Maybe (Doc, Rev))
updateTransactions f = runCouchDB' $ getDoc thedb theDoc >>= go
    where
        go :: Maybe (Doc, Rev, Transactions) -> CouchMonad (Maybe (Doc, Rev))
        go (Just (d, r, ts)) =  updateDoc thedb (d, r) (app ts)
        go _ = fail "get failed"
        app (Transactions ts) = Transactions (f ts)

addTransaction :: Transaction -> IO (Maybe (Doc, Rev))
addTransaction t = updateTransactions (t:)

-- Atomic unlike mapM addTransaction ts
addTransactions :: [Transaction] -> IO (Maybe (Doc, Rev))
addTransactions ts = updateTransactions (ts ++)

deleteTransaction :: Transaction -> IO (Maybe (Doc, Rev))
deleteTransaction t = updateTransactions (delete t)

updateTransaction :: Transaction -> Transaction -> IO (Maybe (Doc, Rev))
updateTransaction old new = updateTransactions ((new:) . delete old)

-- This is horrible. why doesn't isn't CouchMonad a Functor ?
getTransactions :: IO (Maybe [Transaction])
getTransactions = runCouchDB' $ getDoc thedb theDoc >>= unwrapTransactions
    where
        unwrapTransactions (Just (_, _, Transactions ts)) = return $ Just ts
        unwrapTransactions Nothing = return Nothing
