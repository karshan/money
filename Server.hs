{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Applicative ((<$>))
import Control.Monad (void, when)
import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy.Char8 as LBS (pack)
import Data.List ((\\))
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Filesystem.Path.CurrentOS as FP (decodeString)
import Language.Javascript.JMacro
import qualified ManageDB as MDB (getTransactions, addTransaction, deleteTransaction, updateTransaction, updateTransactions)
import qualified Money as M (Transaction, tags, similarTransactions, monthStats)
import Network.HTTP.Types.Header (hContentLength)
import Network.Wai (Request, Response, pathInfo)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Util (mapHeaders)
import Util (jsonApp, redirect, responseLBS', responseJSON, responseString)

main :: IO ()
main = putStrLn ("Listening on port " ++ show port) >> run port app
    where
        port = 3000

app :: Request -> IO Response
app req = route (pathInfo req) req

route :: [Text] -> Request -> IO Response
route path
    | null path = redirect "/static/index.html"
    | head path == "static" = static
    | path == ["transactions"] = transactions
    | path == ["similar"] = similarTransactions
    | path == ["delete"] = deleteTransaction
    | path == ["add"] = addTransaction
    | path == ["update"] = updateTransaction
    | path == ["updateTags"] = updateTags
    | path == ["monthStats"] = monthStats
    | path == ["index.js"] = jMacroApp indexJS
    | otherwise = notFound

notFound :: Request -> IO Response
notFound req = return $ responseString $ "not found " ++ show (pathInfo req)

-- assumes request pathInfo is non-empty. Verified since its checked in route.
-- It would be nice if the compiler could make that guarantee.
static :: Request -> IO Response
static req = mapHeaders (filter (\(k,_) -> k /= hContentLength)) <$> a (req { pathInfo = tail (pathInfo req) })
    where
        a = staticApp $ defaultWebAppSettings $ FP.decodeString "./static"

-- TODO error reporting
ts :: IO [M.Transaction]
ts = fromMaybe [] <$> MDB.getTransactions

transactions :: Request -> IO Response
transactions _ = responseJSON <$> ts

similarTransactions :: Request -> IO Response
similarTransactions = jsonApp (\t -> M.similarTransactions t <$> ts)

-- TODO error reporting
deleteTransaction :: Request -> IO Response
deleteTransaction = jsonApp (void . MDB.deleteTransaction)

-- TODO error reporting
addTransaction :: Request -> IO Response
addTransaction = jsonApp (void . MDB.addTransaction)

-- TODO error reporting
updateTransaction :: Request -> IO Response
updateTransaction = jsonApp (\a -> when (length a == 2) $ void $ MDB.updateTransaction (head a) (a !! 1))

updateTags :: Request -> IO Response
updateTags = jsonApp $ \(sts, tags) -> void $ MDB.updateTransactions $
                            \ats -> (ats \\ sts) ++ map (\t -> t { M.tags = splitOn " " tags }) sts

monthStats :: Request -> IO Response
monthStats _ = (responseJSON . M.monthStats) <$> ts

jMacroApp :: JStat -> Request -> IO Response
jMacroApp js _ = return $ responseLBS' "application/javascript" $ LBS.pack $ show $ renderJs js

indexJS :: JStat
indexJS = jmPrelude <> utils <> [jmacro|

renderer = {};
window.onload = \ -> renderer["overview"]();

renderer["overview"] = \ ->
    jsonGet "/monthStats" \stats -> $("#container").html <|
        concat <| mapFilter (\[_, amt] -> amt != 0)
                    (\[tag, amt] -> wDiv 1920 (fwDiv 500 tag + fwDiv 500 amt)) stats 
|]
    where
        utils = [jmacro|

fun html tag attrs style c -> "<" + tag + " "
               + (concat <| map (\[k, v] -> k + "=" + v + " ") attrs)
               + "style=\"" + (concat <| map (\[k, v] -> k + ": " + v + "; ") style) + "\">"
               + c + "</" + tag + ">";

fun wDiv w s -> html "div" [] [["width", w + "px"]] s;
fun fwDiv w s -> html "div" [] [["width", w + "px"], ["float", "left"]] s;

jsonGet = function(url, cb) {
    var xhr = new XMLHttpRequest();
    xhr.onload = function() {
        if (this.status != 200) {
            cb(null);
        } else {
            var json = null;
            try {
                json = JSON.parse(this.responseText);
            } catch(_) {
                json = null;
            }
            cb(json);
        }
    };
    xhr.open("get", url, true);
    xhr.send();
};

jsonPost = function(url, d, cb) {
    var xhr = new XMLHttpRequest();
    xhr.onload = function() {
        if (this.status != 200) {
            cb(null);
        } else {
            var json = null;
            try {
                json = JSON.parse(this.responseText);
            } catch(_) {
                json = null;
            }
            cb(json);
        }
    };
    xhr.open("post", url, true);
    xhr.send(JSON.stringify(d));
};
|]

