import Control.Monad.Trans.Either
import Control.Monad.IO.Class

import Data.Default.Class
import Reflex.Dom

import Money.API
import ClientAPI (getCreds, addCred, getTransactions, evaluate)

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
      addCredWidget = do
        el "div" $ do
        text "user"
        user_textInput <- textInput def
        el "div" $ do
        text "password"
        password_textInput <- textInput def
        el "div" $ do
        addCred_evt <- button "add cred"
        let userpassword_evt = attachDyn (_textInput_value password_textInput) $ tagDyn (_textInput_value user_textInput) addCred_evt
        res <- evaluate (\(p, u) -> fmap (either (const "error") show) $ runEitherT $ addCred $ BankOfAmericaCreds $ Cred u p []) userpassword_evt
        dynRes <- holdDyn "yoyo do smn" res
        el "div" (dynText dynRes)
        return (const () <$> res)
