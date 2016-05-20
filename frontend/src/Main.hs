{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad.Trans.Either (runEitherT)
import Control.Monad.IO.Class (liftIO)

--import Data.Default.Class (def)
import Data.Function (on)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Reflex.Dom
--import Reflex.Dom.Widget.Basic

import Money.API
import ClientAPI (getCreds, addCred, getTransactions, evaluate)

-- UTIL
styleStr :: [(String, String)] -> String
styleStr = concatMap (\(a,b) -> a ++ ": " ++ b ++ ";") 

elStyle :: MonadWidget t m => String -> [(String, String)] -> m a -> m a
elStyle elementTag style = elAttr elementTag (Map.singleton "style" (styleStr style))

divStyle :: MonadWidget t m => [(String, String)] -> m a -> m a
divStyle s = elStyle "div" s

textInput' :: MonadWidget t m => String -> [(String, String)] -> [(String, String)] -> m (Dynamic t String)
textInput' typ attrs style =
  let 
    attrs' = attrs ++ [("style", styleStr style)]
    t = textInput (def &
          textInputConfig_attributes .~ constDyn (Map.fromList attrs') &
          textInputConfig_inputType .~ typ)
  in _textInput_value <$> t

field :: MonadWidget t m => String -> String -> m (Dynamic t String)
field typ name =
  textInput' typ
    [ ("placeholder", name) ]
    []

textField :: MonadWidget t m => String -> m (Dynamic t String)
textField = field "text"

passwordField :: MonadWidget t m => String -> m (Dynamic t String)
passwordField = field "password"

mkWidget :: MonadWidget t m =>
  model ->
  (action -> model -> model) ->
  (Dynamic t model -> m (Event t action, Event t retval)) ->
  m (Event t retval)
mkWidget initial update view = do
  rec (changes, retval) <- view model
      model <- foldDyn update initial changes
  return retval

showAttr :: Bool -> Map String String
showAttr True = Map.singleton "style" "display: block"
showAttr False = Map.singleton "style" "display: none"

-- A widget that shows one widget out of a list of widgets keyed by a page type.
pages :: (MonadWidget t m, Eq page) => page -> Event t page -> [(m (), page)] -> m ()
pages initialPage pageSelectionE pagelist = do
  let pageDiv widget attrs = elDynAttr "div" attrs widget
  selectedPage <- holdDyn initialPage pageSelectionE
  mapM_ (\(widget, page) -> pageDiv widget =<< mapDyn (showAttr . (== page)) selectedPage) pagelist

imgButton :: MonadWidget t m => String -> m (Event t ())
imgButton src = do
  (e, _) <- elAttr' "input" (Map.fromList
    [ ("type", "image")
    , ("src", src)
    , ("style", styleStr [ ("outline", "none") ])
    ]) $ text ""
  return $ domEvent Click e
-- END UTIL

-- API
addCred' :: Credential -> IO String
addCred' = fmap (either (const "todo show ServantError") show) . runEitherT . addCred
-- END API

-- TODO see if this can be rewritten using mkWidget
addCredPage :: MonadWidget t m => m ()
addCredPage = mdo
  user <- textField "user"
  pass <- passwordField "password"
  addSecret <- button "add secret"
  removeSecret <- button "remove secret"
  secrets :: Dynamic t (Map.Map Int (Dynamic t (String, String))) <- listHoldWithKey Map.empty 
    (
      (\(map', add) -> 
        if add then 
          Map.singleton (Map.size map') (Just ()) 
        else 
          Map.singleton (Map.size map' - 1) Nothing
      ) <$> 
          mergeWith (\a _ -> a) 
            [ attachDyn secrets $ fmap (const True) addSecret
            , attachDyn secrets $ fmap (const False) removeSecret
            ]
    ) 
    (\_ _ -> do
      d1 <- ((,) <$> textField "question" <*> textField "answer")
      (uncurry (combineDyn (,))) d1)
  submit <- button "submit"
  response <- evaluate (\(secrets', (user', pass')) ->
    addCred' $ BankOfAmericaCreds $ Cred user' pass' (map snd $ Map.toList secrets')) $
       attachDyn (joinDynThroughMap secrets) $ attachDyn user $ tagDyn pass submit
  el "div" (holdDyn "" response >>= dynText)
  pb <- getPostBuild
  res <- evaluate (\_ -> fmap (either (const "error") show) $ runEitherT $ getCreds) (leftmost [ const () <$> response, pb ])
  dynRes <- holdDyn "waiting..." res
  el "div" $ dynText dynRes

menuButton :: MonadWidget t m => m (Event t ())
menuButton = imgButton "/static/images/ic_menu_24px.svg"

backButton :: MonadWidget t m => m (Event t ())
backButton = imgButton "/static/images/ic_arrow_back_24px.svg"

data Page = PageAddCred | PageViewTransactions deriving (Eq, Show, Ord)

menuPane :: MonadWidget t m => m (Event t (), Event t Page)
menuPane = do
  divStyle
    [ ("position", "absolute")
    , ("z-index", "1")
    , ("left", "0")
    , ("top", "0")
    , ("width", "320px")
    , ("height", "100%")
    , ("background-color", "white")
    , ("box-shadow", "0 -1px 24px rgba(0,0,0,0.4)")
    ] $ do
  backEvt <- padded backButton
  pageAddCred <- padded $ button "add credentials"
  pageViewTransactions <- padded $ button "view transactions"
  return (leftmost [ backEvt, pageAddCred, pageViewTransactions], leftmost
    [ const PageAddCred <$> pageAddCred
    , const PageViewTransactions <$> pageViewTransactions])
    where
      padded = divStyle [("padding", "6px")]

menuWidget :: MonadWidget t m => m (Event t Page)
menuWidget =
  mkWidget False (\action _ -> action) (\model -> do
    (hideMenu, pageEvt) <- mapDyn showAttr model >>= (\attr ->
      elDynAttr "div" attr $ menuPane)
    showMenu <- el "div" $ menuButton
    return $ (leftmost [const True <$> showMenu, const False <$> hideMenu], pageEvt))

showAmt :: Int -> String
showAmt amt =
  let str = show amt
  in take (length str - 2) str ++ "." ++ drop (length str - 2) str

transactionsPage :: MonadWidget t m => m ()
transactionsPage = do
  let td' s = elStyle "td" ([("padding", "8px")] ++ s)
  let tr' s = elStyle "tr" ([("border-bottom", "1px solid"), ("border-color", "inherit")] ++ s)
  let table' = elStyle "table" [("border", "1px solid #ccc"), ("border-collapse", "collapse"), ("width", "100%")]
  ts <- liftIO $ runEitherT getTransactions
  either (const $ text "todo show ServantError") (\(_hash, transactions) -> do
    table' $ do
      flip mapM_ (zip [(1 :: Int)..] (sortBy (flip compare `on` date) transactions)) (\(i, (Transaction desc dte amt)) -> do
        tr' [("background-color", if i `mod` 2 == 0 then "#ffffff" else "#f1f1f1")] $ do
          td' [] $ text $ show dte
          td' [] $ text $ desc
          td' [("text-align", "right")] $ text $ showAmt amt)) ts

main :: IO ()
main = do
  mainWidget $ el "div" $ do
  pageSelectionE <- menuWidget
  el "div" $ do
    pages PageViewTransactions pageSelectionE
      [ (addCredPage, PageAddCred)
      , (transactionsPage, PageViewTransactions)
      ]
