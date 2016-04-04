{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrapers.Browser
    ( Browser
    , Error (..)
    , LogRecord
    , RequestLog (..)
    , ResponseLog (..)
    , get
    , getWith
    , post
    , postWith
    , runBrowser
    ) where

import           Control.Exception        (SomeException, try)
import           Control.Lens             ((%~), (&), (.~), (^.))
import           Control.Monad.Except     (ExceptT (..), MonadError, runExceptT,
                                           throwError)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.RWS.Strict (MonadReader, MonadState, MonadWriter,
                                           RWST (..), ask, tell)
import qualified Control.Monad.State      as S (get, put)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Lazy     as LBS (ByteString)
import           Data.List                (nub)
import           Data.Monoid              ((<>))
import           Network.HTTP.Client      (CookieJar)
import           Network.Wreq             (Options, Response, auth, cookies,
                                           headers, responseCookieJar)
import qualified Network.Wreq             as W (getWith, postWith)
import           Prelude                  hiding (last, (!!))
import           Scrapers.Browser.Log     (LogRecord, RequestLog (..),
                                           ResponseLog (..), mkPost,
                                           mkResponseLog)

data Error = WreqException SomeException | MyError String deriving (Show)

newtype Browser a = Browser { runBr :: ExceptT Error (RWST Options [LogRecord] CookieJar IO) a }
    deriving (Functor, Applicative, Monad, MonadReader Options, MonadState CookieJar, MonadWriter [LogRecord], MonadIO, MonadError Error)

runBrowser :: CookieJar -> Options -> Browser a -> IO (Either Error a, CookieJar, [LogRecord])
runBrowser cj o br = runRWST (runExceptT $ runBr br) o cj

-- lift functions from Wreq in IO to Browser
liftBrowser :: RequestLog -> (Options -> IO (Response LBS.ByteString)) -> Browser (Response LBS.ByteString)
liftBrowser reqLog f = do
    options <- ask
    cookieJar <- S.get
    response <- either (throwError . WreqException) return =<<
                    liftIO (try (f (options & cookies .~ Just cookieJar)))
    tell [(reqLog, mkResponseLog response)]
    S.put (cookieJar <> response ^. responseCookieJar)
    return response

post :: String -> [(ByteString, ByteString)] -> Browser (Response LBS.ByteString)
post url params = liftBrowser (mkPost url params) (\o -> W.postWith o url params)

get :: String -> Browser (Response LBS.ByteString)
get url = liftBrowser (Get url) (`W.getWith` url)

getWith :: Options -> String -> Browser (Response LBS.ByteString)
getWith options url = liftBrowser (Get url) (\o -> W.getWith (mixOptions o options) url)

postWith :: Options -> String -> [(ByteString, ByteString)] -> Browser (Response LBS.ByteString)
postWith options url params = liftBrowser (mkPost url params) (\o -> W.postWith (mixOptions o options) url params)

mixOptions :: Options -> Options -> Options
mixOptions global transient =
    global & auth %~ maybe (global ^. auth) (const $ transient ^. auth)
           & headers .~ nub (global ^. headers <> transient ^. headers) -- TODO nubBy HeaderKey and prefer headers from transient
           & cookies .~ (global ^. cookies <> transient ^. cookies)
