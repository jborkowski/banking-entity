{-# LANGUAGE OverloadedStrings #-}

-- import Control.Monad (replicateM)

module TransactionsSpec where

import Api.Account (createAccount)
import Api.Operations (_transfer, _withdraw, checkBalance, deposit)
import Config
  ( App,
    AppT (..),
    Config (..),
  )
import Control.Concurrent (forkIO, takeMVar)
import Control.Concurrent.MVar -- (newMVar, putMVar, readMVar)
import Control.Concurrent.STM (atomically, newTVarIO, readTVar)
import Control.Exception (throwIO)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Models hiding (balance)
import Servant
import Test.Hspec (Spec, anyException, describe, it, parallel, shouldBe, shouldReturn, shouldThrow)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

runAppToIO :: Accounts -> App a -> IO a
runAppToIO state app = do
  config <- newMVar state
  result <- runExceptT $ runReaderT (runApp app) $ Config config
  case result of
    Left err -> throwIO err
    Right a -> return a

joeyUser = User {_email = "joey.tribbiani@mail.com", _firstName = "Joey", _lastName = "Tribbiani"}

joeyAccountName = _email joeyUser

spec :: Spec
spec = do
  describe "Bank Account operations" $ do
    it "can parse integers - smoke test" $
      read "10" `shouldBe` (10 :: Int)
    it "should throw SeverError for unknown userName" $
      runAppToIO M.empty (checkBalance (Just "unknown UserName")) `shouldThrow` (== ServerError {errHTTPCode = 400, errReasonPhrase = "Bad Request", errBody = "Cannot find account with provided name", errHeaders = []})
    --
    it "should open new bank account" $ do
      balance <- runAppToIO M.empty $ do
        createAccount joeyUser
        --deposit OperationForm {_accountName = joeyAccountName, _amount = 20}
        checkBalance (Just joeyAccountName)
      balance `shouldBe` 20
--
-- it "leave deposit" $ do
--   balance <- runAppToIO M.empty $ do
--     createAccount joeyUser
--     deposit OperationForm {_accountName = joeyAccountName, _amount = 20}
--     checkBalance (Just joeyAccountName)
--   balance `shouldBe` 20
