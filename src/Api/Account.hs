{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Account where

import Config (AppT (..), Config (..))
import Control.Concurrent.MVar (putMVar, readMVar)
import Control.Concurrent.STM (atomically, newTVarIO, readTVar)
import Control.Monad.Except (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import qualified Data.Map.Strict as M (insert, lookup)
import Models
import Servant

type GetAccountData = QueryParam "accountName" String :> Get '[JSON] AccountData

type CreateAccount = ReqBody '[JSON] User :> PostCreated '[JSON] ()

type AccountAPI = "account" :> (GetAccountData :<|> CreateAccount)

accountApi :: Proxy AccountAPI
accountApi = Proxy

accountServer :: (MonadIO m) => ServerT AccountAPI (AppT m)
accountServer = showAccount :<|> createAccount

createAccount :: (MonadIO m) => User -> AppT m ()
createAccount newUser = do
  u <- liftIO $ newTVarIO (emptyAccount newUser)
  modifyAccounts (M.insert (genAccountName newUser) u)

genAccountName :: User -> AccountName
genAccountName = _email

modifyAccounts :: (MonadReader Config m, MonadIO m) => (Accounts -> Accounts) -> m ()
modifyAccounts fn = do
  state <- asks accounts
  liftIO $ readMVar state >>= putMVar state . fn

readAccounts :: (MonadReader Config m, MonadIO m) => m Accounts
readAccounts = do
  state <- asks accounts
  liftIO $ readMVar state

showAccount :: (MonadIO m) => Maybe String -> AppT m AccountData
showAccount (Just aname) = do
  a <- readAccounts
  maybeAccount <- liftIO $ atomically $ traverse readTVar $ M.lookup aname a
  case maybeAccount of
    Nothing ->
      throwError $ err400 {errBody = "Account with provided name doesn't exists"}
    Just account ->
      return account
showAccount Nothing =
  throwError err400 {errBody = "Missing 'account name' param"}
