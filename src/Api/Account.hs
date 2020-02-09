{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Account where

import Config (AppT (..), Config (..))
import Control.Concurrent.STM (atomically, readTVar, writeTVar)
import Control.Monad.Except (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import qualified Data.Map as M (insert, lookup)
import Models
import Servant

type GetAccount = QueryParam "accountName" String :> Get '[JSON] Account

type AddAccount = ReqBody '[JSON] User :> PostCreated '[JSON] ()

type AccountAPI = "account" :> (GetAccount :<|> AddAccount)

accountApi :: Proxy AccountAPI
accountApi = Proxy

accountServer :: (MonadIO m) => ServerT AccountAPI (AppT m)
accountServer = getAccount :<|> addAccount

addAccount :: (MonadIO m) => User -> AppT m ()
addAccount = _addAccount

_addAccount :: (MonadReader Config m, MonadIO m) => User -> m ()
_addAccount newUser = do
  a <- asks accounts
  liftIO $ atomically $ readTVar a >>= writeTVar a . M.insert (_email newUser) (emptyAccount newUser)

getAccount :: (MonadIO m) => Maybe String -> AppT m Account
getAccount (Just name) = do
  maybeAccount <- _getAccount (name)
  case maybeAccount of
    Nothing ->
      throwError $ err400 {errBody = "Account with provided name doesn't exists"}
    Just account ->
      return account
getAccount Nothing =
  throwError err400 {errBody = "To find account informations, please provide account name"}

_getAccount :: (MonadReader Config m, MonadIO m) => String -> m (Maybe Account)
_getAccount aname = do
  a <- asks accounts
  liftIO $ atomically $ M.lookup aname <$> readTVar a
