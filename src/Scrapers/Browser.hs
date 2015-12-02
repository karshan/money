{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrapers.Browser
    ( Browser
    , get
    , getWith
    , post
    , postWith
    , runBrowser
    , runBrowserWithLog
    ) where

import           Control.Lens                ((%~), (&), (.~), (^.))
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader        (MonadReader, ReaderT (..), ask)
import           Control.Monad.State         (MonadState, StateT (..))
import qualified Control.Monad.State         as S (get, put)
import           Control.Monad.Writer.Strict (MonadWriter, WriterT (..), tell)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Lazy.Char8  as LBS (ByteString)
import           Data.Default.Class          (def)
import           Data.List                   (nub)
import           Data.Monoid                 ((<>))
import           Network.HTTP.Client         (CookieJar)
import           Network.Wreq                (Options, Response, auth,
                                              checkStatus, cookies, headers,
                                              responseCookieJar)
import qualified Network.Wreq                as W (getWith, postWith)

import           Prelude                     hiding (last, (!!))

data RequestLog = Get String | Post String [(ByteString, ByteString)] deriving (Eq, Ord, Show)
type LogRecord a = (RequestLog, Response a)

-- TODO Documentation
newtype Browser a = Browser { runBr :: WriterT [LogRecord LBS.ByteString] (ReaderT Options (StateT CookieJar IO)) a }
    deriving (Functor, Applicative, Monad, MonadReader Options, MonadState CookieJar, MonadWriter [LogRecord LBS.ByteString], MonadIO)

runBrowser :: Options -> Browser a -> IO a
runBrowser o = fmap fst . runBrowserWithLog o

runBrowserWithLog :: Options -> Browser a -> IO (a, [LogRecord LBS.ByteString])
runBrowserWithLog o br =
    fst <$> runStateT (runReaderT (runWriterT $ runBr br) options) def
        where
            options = o & checkStatus .~ Just (\_ _ _ -> Nothing)

-- lift functions from Wreq in IO to Browser
liftBrowser :: RequestLog -> (Options -> IO (Response LBS.ByteString)) -> Browser (Response LBS.ByteString)
liftBrowser reqLog f = do
    options <- ask
    cookieJar <- S.get
    response <- liftIO (f (options & cookies .~ Just cookieJar))
    tell [(reqLog, response)]
    S.put (cookieJar <> response ^. responseCookieJar)
    return response

post :: String -> [(ByteString, ByteString)] -> Browser (Response LBS.ByteString)
post url params = liftBrowser (Post url params) (\o -> W.postWith o url params)

get :: String -> Browser (Response LBS.ByteString)
get url = liftBrowser (Get url) (\o -> W.getWith o url)

getWith :: Options -> String -> Browser (Response LBS.ByteString)
getWith options url = liftBrowser (Get url) (\o -> W.getWith (mixOptions o options) url)

postWith :: Options -> String -> [(ByteString, ByteString)] -> Browser (Response LBS.ByteString)
postWith options url params = liftBrowser (Post url params) (\o -> W.postWith (mixOptions o options) url params)

-- TODO type safety: differentiate types of transient and global
mixOptions :: Options -> Options -> Options
mixOptions transient global =
    global & auth %~ maybe (global ^. auth) (const $ transient ^. auth)
           & headers .~ nub (global ^. headers <> transient ^. headers)
           & cookies .~ (global ^. cookies <> transient ^. cookies)
