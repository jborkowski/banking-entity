{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Operations where

import Config (AppT (..), Config (..))
import Control.Concurrent.STM (TVar, atomically, newTVar, readTVar, readTVarIO, writeTVar)
import Control.Lens
import Control.Monad.Except (MonadIO, liftIO)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, ask, asks)
import qualified Data.Map as M (insert, lookup)
import Models
import Servant

type Deposit = "deposit" :> ReqBody '[JSON] DepositForm :> PostCreated '[JSON] ()

type CheckBalance = "balance" :> QueryParam "accountName" String :> Get '[JSON] Balance

type OperationsAPI = Deposit :<|> CheckBalance

operationsApi :: Proxy OperationsAPI
operationsApi = Proxy

operationsServer :: (MonadIO m) => ServerT OperationsAPI (AppT m)
operationsServer = deposit :<|> checkBalance

deposit :: (MonadIO m) => DepositForm -> AppT m ()
deposit = undefined

_deposit :: (MonadReader Config m, MonadIO m) => DepositForm -> m ()
_deposit form = do
  a <- asks accounts
  let accountName = _aName form
  aLs <- liftIO $ readTVarIO a
  maybeAccount <- liftIO $ atomically $ M.lookup accountName <$> readTVar a
  case maybeAccount of
    Nothing -> undefined
    Just oldAccount ->
      --let updated = oldAccount $ balance %~ subtract (_ammount form)

      undefined
  -- >>= write a . M.insert userName ()
  return ()

--liftIO $ atomically $ M.lookup accountName <$> readTVar a

checkBalance :: (MonadIO m) => Maybe String -> AppT m Balance
checkBalance (Just name) = do
  maybeAccount <- _checkBalance name
  case maybeAccount of
    Nothing ->
      throwError $ err400 {errBody = "Account with provided name doesn't exists"}
    Just b ->
      return b
checkBalance Nothing =
  throwError err400 {errBody = "To check account balance, please provide account name"}

_checkBalance :: (MonadReader Config m, MonadIO m) => String -> m (Maybe Balance)
_checkBalance aname = do
  a <- asks accounts
  liftIO $ atomically $ fmap _balance . M.lookup aname <$> readTVar a
