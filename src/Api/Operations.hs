{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Operations where

import Config (AppT (..), Config (..))
import Control.Concurrent.MVar (readMVar)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar, readTVarIO)
import Control.Lens ((+~), (-~))
import Control.Monad.Except (MonadError, MonadIO, liftIO)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, ask, asks)
import qualified Data.Map as M (Map, lookup)
import Models hiding (amount)
import Servant

type Deposit = "deposit" :> ReqBody '[JSON] OperationForm :> PostCreated '[JSON] ()

type CheckBalance = "balance" :> QueryParam "accountName" String :> Get '[JSON] Balance

type OperationsAPI = Deposit :<|> CheckBalance

operationsApi :: Proxy OperationsAPI
operationsApi = Proxy

operationsServer :: (MonadIO m) => ServerT OperationsAPI (AppT m)
operationsServer = deposit :<|> checkBalance

deposit :: (MonadIO m) => OperationForm -> AppT m ()
deposit OperationForm {_amount = a, _accountName = n} =
  readAccount n >>= liftIO . atomically . _deposit a

_deposit :: Int -> Account -> STM ()
_deposit amount account = modifyTVar account (balance +~ amount)

withdraw :: (MonadIO m) => OperationForm -> AppT m ()
withdraw OperationForm {_amount = a, _accountName = n} =
  readAccount n >>= liftIO . atomically . _withdraw a

_withdraw :: Int -> Account -> STM ()
_withdraw amount account = modifyTVar account (balance -~ amount)

checkBalance :: (MonadIO m) => Maybe String -> AppT m Balance
checkBalance (Just n) = do
  a <- readAccount n
  acc <- liftIO $ readTVarIO a
  return $ _balance acc
checkBalance Nothing =
  throwError err400 {errBody = "To check account balance, please provide account name"}

transfer :: (MonadIO m) => AccountName -> AccountName -> Int -> AppT m ()
transfer from to amount = do
  fromAccount <- readAccount from
  toAccount <- readAccount to
  liftIO $ _transfer fromAccount toAccount amount

_transfer :: (MonadIO m) => Account -> Account -> Int -> m ()
_transfer from to amount =
  liftIO $ atomically $
    ( do
        _withdraw amount from
        _deposit amount to
    )

readAccount :: (MonadReader Config m, MonadIO m, MonadError ServerError m) => AccountName -> m Account
readAccount n = do
  stateVar <- asks accounts
  state <- liftIO $ readMVar stateVar
  case M.lookup n state of
    Nothing -> throwError err400 {errBody = "Cannot find account with provided name"}
    Just account -> return account
