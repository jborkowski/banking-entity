{-# LANGUAGE OverloadedStrings #-}

-- import Control.Monad (replicateM)

module TransactionsSpec where

import Api.Account (createAccount, showAccount)
import Api.Operations (checkBalance, deposit, transfer, withdraw)
import Config
  ( App,
    AppT (..),
    Config (..),
  )
import Control.Concurrent (forkIO, takeMVar)
import Control.Concurrent.MVar (newMVar)
import Control.Concurrent.STM (atomically, newTVarIO, readTVar)
import Control.Exception (throwIO)
import Control.Monad.Except (liftIO, runExceptT)
import Control.Monad.Reader (runReaderT)
import qualified Data.Map.Strict as M (empty, fromList)
import Models hiding (balance)
import Servant
import Test.Hspec (Spec, anyException, describe, it, parallel, shouldBe, shouldReturn, shouldThrow)

runAppToIO :: Accounts -> App a -> IO a
runAppToIO state app = do
  config <- newMVar state
  result <- runExceptT $ runReaderT (runApp app) $ Config config
  case result of
    Left err -> throwIO err
    Right a -> return a

joeyUser :: User
joeyUser = User {_email = "joey.tribbiani@mail.com", _firstName = "Joey", _lastName = "Tribbiani"}

chandlerUser :: User
chandlerUser = User {_email = "chandler.bing@mail.com", _firstName = "Chendler", _lastName = "Bing"}

joeyAccountName :: AccountName
joeyAccountName = _email joeyUser

chandlerAccountName :: AccountName
chandlerAccountName = _email chandlerUser

joeyJustName :: Maybe AccountName
joeyJustName = Just joeyAccountName

chandlerJustName :: Maybe AccountName
chandlerJustName = Just chandlerAccountName

friendsBankState :: IO Accounts
friendsBankState = do
  joey <- newTVarIO (emptyAccount joeyUser)
  chandler <- newTVarIO (emptyAccount chandlerUser)
  return $ M.fromList [(joeyAccountName, joey), (chandlerAccountName, chandler)]

serverErrorBadRequest :: ServerError
serverErrorBadRequest = ServerError {errHTTPCode = 400, errReasonPhrase = "Bad Request", errBody = "Cannot find account with provided name", errHeaders = []}

spec :: Spec
spec = parallel $ do
  describe "Account Operations" $ do
    it "should open new bank account" $ do
      account <- runAppToIO M.empty $ do
        createAccount joeyUser
        showAccount joeyJustName
      account `shouldBe` (emptyAccount joeyUser)
    it "should create account wiht zero balance" $ do
      balance <- runAppToIO M.empty $ do
        createAccount joeyUser
        checkBalance joeyJustName
      balance `shouldBe` 0
  describe "Deposit and withdraw transactions" $ do
    it "should throw SeverError for unknown userName" $
      runAppToIO M.empty (checkBalance (Just "unknown UserName")) `shouldThrow` (== serverErrorBadRequest)
    it "should deposit $400 and withdraw $80 - Chandler account" $ do
      state <- friendsBankState
      balance <- runAppToIO state $ do
        deposit OperationForm {_accountName = chandlerAccountName, _amount = 400}
        withdraw OperationForm {_accountName = chandlerAccountName, _amount = 80}
        checkBalance chandlerJustName
      balance `shouldBe` 320
    it "should deposit amount into account" $ do
      balance <- runAppToIO M.empty $ do
        createAccount joeyUser
        deposit OperationForm {_accountName = joeyAccountName, _amount = 20}
        checkBalance joeyJustName
      balance `shouldBe` 20
  describe "Transfer money" $ do
    it "Transfer money from Chandler to Joey" $ do
      state <- friendsBankState
      finalBalance <- runAppToIO state $ do
        deposit OperationForm {_accountName = chandlerAccountName, _amount = 400}
        transfer TransferForm {_from = chandlerAccountName, _to = joeyAccountName, _transferAmount = 80}
        jb <- checkBalance joeyJustName
        cb <- checkBalance chandlerJustName
        return (jb, cb)
      fst finalBalance `shouldBe` 80
      snd finalBalance `shouldBe` 320
