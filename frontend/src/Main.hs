{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad.Trans.Either
import Control.Monad.IO.Class

import Data.Default.Class
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Reflex.Dom
--import Reflex.Dom.Widget.Basic

import Money.API
import ClientAPI (getCreds, addCred, getTransactions, evaluate)

styleStr :: [(String, String)] -> String
styleStr = concatMap (\(a,b) -> a ++ ": " ++ b ++ ";") 

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

addCred' :: Credential -> IO String
addCred' = fmap (either (const "todo show ServantError") show) . runEitherT . addCred

-- Map k v -> Event t (Map k (Maybe v)) -> (k -> v -> m a) -> m (Dynamic t (Map k a))
addCredWidget :: MonadWidget t m => m (Event t ())
addCredWidget = mdo
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
  return (const () <$> response)

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

imgButton :: MonadWidget t m => String -> m (Event t ())
imgButton src = do
  (e, _) <- elAttr' "input" (Map.fromList 
    [ ("type", "image")
    , ("src", src)
    , ("style", styleStr [ ("outline", "none") ])
    ]) $ text ""
  return $ domEvent Click e

menuButton :: MonadWidget t m => m (Event t ())
menuButton = imgButton "/static/images/ic_menu_24px.svg"

backButton :: MonadWidget t m => m (Event t ())
backButton = imgButton "/static/images/ic_arrow_back_24px.svg"

divStyle :: MonadWidget t m => [(String, String)] -> m a -> m a
divStyle s = elAttr "div" (Map.singleton "style" (styleStr s))

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
    ] $ do
  backEvt <- el "div" $ backButton
  pageAddCred <- padded $ button "add credentials"
  pageViewTransactions <- padded $ button "view transactions"
  return (leftmost
    [ backEvt
    , const () <$> pageAddCred
    , const () <$> pageViewTransactions
    ], leftmost
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

main :: IO ()
main = do
  mainWidget $ el "div" $ do
  pb <- getPostBuild
  pageEvt <- menuWidget
  el "div" $ (dynText =<< mapDyn show =<< holdDyn PageAddCred pageEvt)
  addCred_evt <- addCredWidget
  el "div" transactionWidget
  el "div" (credsWidget (appendEvents pb addCred_evt))
    where
      transactionWidget = do
        ts <- liftIO $ runEitherT getTransactions
        text (either (const "error") show ts)
      credsWidget e = do
        res <- evaluate (\_ -> fmap (either (const "error") show) $ runEitherT $ getCreds) e
        dynRes <- holdDyn "waiting..." res
        dynText dynRes
