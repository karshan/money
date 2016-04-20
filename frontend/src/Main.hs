{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad.Trans.Either
import Control.Monad.IO.Class

import Data.Default.Class
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

import Reflex.Dom
import Reflex.Dom.Widget.Basic

import Money.API
import ClientAPI (getCreds, addCred, getTransactions, evaluate)

field :: MonadWidget t m => String -> m (Dynamic t String)
field name =
  el "tr" $ do
    el "td" $ text name
    _textInput_value <$> el "td" (textInput def)

addCred' :: Credential -> IO String
addCred' = fmap (either (const "todo show ServantError") show) . runEitherT . addCred

-- Map k v -> Event t (Map k (Maybe v)) -> (k -> v -> m a) -> m (Dynamic t (Map k a))
addCredWidget :: MonadWidget t m => m (Event t ())
addCredWidget = mdo
  (user, pass) <- el "table" $ do
    user <- field "user"
    pass <- field "password"
    return (user, pass)
  addSecret <- button "add secret"
  removeSecret <- button "remove secret"
  secrets :: Dynamic t (Map.Map Int (Dynamic t String)) <- listHoldWithKey Map.empty 
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
    (\_ _ -> _textInput_value <$> textInput def)
  submit <- button "submit"
  response <- evaluate (\(secrets', (user', pass')) ->
    addCred' $ BankOfAmericaCreds $ Cred user' pass' (map (\x -> (x, "")) secrets')) $
      pushAlways (\(secrets', (user', pass')) -> do
          s <- mapM (\i -> sample $ current $ secrets' ! i) [0..Map.size secrets' - 1]
          return (s, (user', pass'))
        ) $ attachDyn secrets $ attachDyn user $ tagDyn pass submit
  el "div" (holdDyn "" response >>= dynText)
  return (const () <$> response)

main :: IO ()
main = do
  mainWidget $ el "div" $ do
  pb <- getPostBuild
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

