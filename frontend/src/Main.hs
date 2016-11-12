{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Main (
    main
) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import GHCJS.DOM -- (enableInspector, webViewGetDomDocument, runWebGUI)
import GHCJS.DOM.Document (Document, getBody, createElement, createTextNode, click, querySelector)
import GHCJS.DOM.Element (Element, IsElement, setInnerHTML, setAttribute)
import GHCJS.DOM.Node (appendChild, IsNode)
import GHCJS.DOM.EventM (on, mouseClientXY)
import GHCJS.DOM.Types (Text)
import Data.JSString (JSString)
import ClientAPI (getTxnDb)
import Control.Monad.Trans.Either (runEitherT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Util (htmlEntities)
import Data.IxSet.Typed
import Data.Maybe (fromMaybe)
import Money.API
import Data.Proxy
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String
import Prelude

renderBlaze :: (MonadIO m, IsElement a) => a -> Html -> m ()
renderBlaze e h =
    setInnerHTML e (Just $ renderHtml h)

renderTxnList :: [Txn] -> Html
renderTxnList ts =
    H.table $ do
        H.tr $ do
            H.th ! A.style alignLeft $ "Date"
            H.th ! A.style alignLeft $ "Description"
            H.th ! A.style alignLeft $ "Amount"
        forM_ ts $ \t ->
            H.tr $ do
                H.td $ H.toHtml $ show $ date t
                H.td $ H.toHtml $ description t
                H.td ! A.style alignRight $ H.toHtml $ show (fromIntegral (amount t) / 100)
    where
        alignLeft = "text-align: left"
        alignRight = "text-align: right"

main :: IO ()
main = run 3708 $ do
    Just doc <- currentDocument
    Just body <- getBody doc
    renderBlaze body (H.div ! A.id "container" ! A.style "" $ "")
    Just container <- querySelector doc "#container"
    eTransactions <- liftIO $ getTxnDb
    either undefined
        (\setTxn -> do
            let txnDb = (fromSet setTxn) :: TxnDb
            renderBlaze container (renderTxnList (toDescList (Proxy :: Proxy Date) txnDb)))
        eTransactions
        {-
    _ <- on doc click $ do
        (x, y) <- mouseClientXY
        Just newParagraph <- createElement doc (Just ("p" :: JSString))
        text <- createTextNode doc $ "Click " ++ show (x, y)
        _ <- appendChild newParagraph text
        _ <- appendChild body (Just newParagraph)
        return ()
    return () -}
