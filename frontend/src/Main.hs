module Main (
    main
) where

import GHCJS.DOM -- (enableInspector, webViewGetDomDocument, runWebGUI)
import GHCJS.DOM.Document (getBody, createElement, createTextNode, click)
import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.Node (appendChild)
import GHCJS.DOM.EventM (on, mouseClientXY)
import ClientAPI (getTransactions)
import Control.Monad.Trans.Either (runEitherT)
import Control.Monad.IO.Class (liftIO)
import Util (htmlEntities)

main :: IO ()
main = runWebGUI $ \ webView -> do
    Just doc <- webViewGetDomDocument webView
    Just body <- getBody doc
    setInnerHTML body (Just "<h1>Loading</h1>")
    eTransactions <- liftIO $ runEitherT getTransactions
    either (\_ -> setInnerHTML body (Just "ServantError..."))
        (\o -> setInnerHTML body (Just $ htmlEntities $ show o))
        eTransactions
    _ <- on doc click $ do
        (x, y) <- mouseClientXY
        Just newParagraph <- createElement doc (Just "p")
        text <- createTextNode doc $ "Click " ++ show (x, y)
        _ <- appendChild newParagraph text
        _ <- appendChild body (Just newParagraph)
        return ()
    return ()
