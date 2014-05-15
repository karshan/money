module ManageDB 
    ( ManageDB.init
    , deleteAll
    , addTransaction
    , getTransactions
    )
    where

import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Database.CouchDB (DB, Doc, Rev, runCouchDB', createDB, dropDB, newDoc, db, getAllDocIds, getDoc)
import Money (Transaction)

dbName :: String
dbName = "transactions"

-- TODO better name ?
thedb :: DB
thedb = db dbName

-- TODO reimplement this as a Monad with runDB so that it
-- is unnecessary to call init

-- If this fails CouchDB is down or the db already exists 
-- either way we don't care. (I guess we kinda care if 
-- couchDB is down.... TODO)
init :: IO ()
init = runCouchDB' (createDB dbName) `catch` (const (return ()) :: SomeException -> IO ())

-- TODO handle exceptions/failure
deleteAll :: IO ()
deleteAll = void $ runCouchDB' $ dropDB dbName

addTransaction :: Transaction -> IO (Doc, Rev)
addTransaction t = runCouchDB' $ newDoc thedb t

getTransactions :: IO ([Maybe (Doc, Rev, Transaction)])
getTransactions = runCouchDB' (getAllDocIds (db dbName) >>= mapM (getDoc thedb))
