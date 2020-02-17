{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Operations where

import Config (AppT (..), Config (..))
import Control.Concurrent.MVar (readMVar)
import Control.Concurrent.STM (STM, atomically, modifyTVar, readTVarIO)
import Control.Lens ((+~), (-~))
import Control.Monad.Except (MonadError, MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import qualified Data.Map as M (lookup)
import Models hiding (amount)
import Servant

type Deposit = "deposit" :> ReqBody '[JSON] OperationForm :> PostCreated '[JSON] NoContent

type Withdraw = "withdraw" :> ReqBody '[JSON] OperationForm :> PostCreated '[JSON] NoContent

type Transfer = "transfer" :> ReqBody '[JSON] TransferForm :> PostCreated '[JSON] NoContent

type CheckBalance = "balance" :> QueryParam "accountName" String :> Get '[JSON] Balance

type OperationsAPI = Deposit :<|> Withdraw :<|> Transfer :<|> CheckBalance

operationsApi :: Proxy OperationsAPI
operationsApi = Proxy

operationsServer :: (MonadIO m) => ServerT OperationsAPI (AppT m)
operationsServer = deposit :<|> withdraw :<|> transfer :<|> checkBalance

deposit :: (MonadIO m) => OperationForm -> AppT m NoContent
deposit OperationForm {_amount = a, _accountName = n} = do
  readAccount n >>= liftIO . atomically . _deposit a
  return NoContent

_deposit :: Int -> Account -> STM ()
_deposit amount account = modifyTVar account (balance +~ amount)

withdraw :: (MonadIO m) => OperationForm -> AppT m NoContent
withdraw OperationForm {_amount = a, _accountName = n} = do
  readAccount n >>= liftIO . atomically . _withdraw a
  return NoContent

_withdraw :: Int -> Account -> STM ()
_withdraw amount account = modifyTVar account (balance -~ amount)

checkBalance :: (MonadIO m) => Maybe String -> AppT m Balance
checkBalance (Just n) = do
  a <- readAccount n
  acc <- liftIO $ readTVarIO a
  return $ _balance acc
checkBalance Nothing =
  throwError err400 {errBody = "To check account balance, please provide account name"}

transfer :: (MonadIO m) => TransferForm -> AppT m NoContent
transfer TransferForm {_from = from, _to = to, _transferAmount = amount} = do
  fromAccount <- readAccount from
  toAccount <- readAccount to
  liftIO $ atomically $ do
    _withdraw amount fromAccount
    _deposit amount toAccount
  return NoContent

readAccount :: (MonadReader Config m, MonadIO m, MonadError ServerError m) => AccountName -> m Account
readAccount n = do
  stateVar <- asks accounts
  state <- liftIO $ readMVar stateVar
  case M.lookup n state of
    Nothing -> throwError err400 {errBody = "Cannot find account with provided name"}
    Just account -> return account
