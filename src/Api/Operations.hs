{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Operations where

import Config (AppT (..), Config (..))
import Control.Concurrent.STM (STM, TVar, atomically, check, newTVar, readTVar, readTVarIO, writeTVar)
import Control.Lens
import Control.Monad.Except (MonadIO, liftIO)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, ask, asks)
import qualified Data.Map as M (Map, adjust, insert, lookup)
import Models
import Servant

type Deposit = "deposit" :> ReqBody '[JSON] OperationForm :> PostCreated '[JSON] ()

type CheckBalance = "balance" :> QueryParam "accountName" String :> Get '[JSON] Balance

type OperationsAPI = Deposit :<|> CheckBalance

operationsApi :: Proxy OperationsAPI
operationsApi = Proxy

operationsServer :: (MonadIO m) => ServerT OperationsAPI (AppT m)
operationsServer = undefined
-- operationsServer = deposit :<|> checkBalance

-- deposit :: (MonadIO m) => OperationForm -> AppT m ()
-- deposit OperationForm {_ammount = a, _accountName = n} =
--   updateState depositSTM a n

-- withdraw :: (MonadIO m) => OperationForm -> AppT m ()
-- withdraw OperationForm {_ammount = a, _accountName = n} =
--   updateState depositSTM a n

-- updateState :: (MonadReader Config m, MonadIO m) => (Int -> AccountName -> Accounts -> STM ()) -> Int -> AccountName -> m ()
-- updateState fn a n =
--   asks accounts >>= liftIO . atomically . fn a n

-- depositSTM :: Int -> AccountName -> Accounts -> STM ()
-- depositSTM a n state =
--   readTVar state >>= writeTVar state . M.adjust (balance +~ a) n

-- withdrawSTM :: Int -> AccountName -> Accounts -> STM ()
-- withdrawSTM a n state =
--   readTVar state >>= writeTVar state . M.adjust (balance -~ a) n

-- checkBalance :: (MonadIO m) => Maybe String -> AppT m Balance
-- checkBalance (Just name) = do
--   maybeAccount <- _checkBalance name
--   case maybeAccount of
--     Nothing ->
--       throwError $ err400 {errBody = "Account with provided name doesn't exists"}
--     Just b ->
--       return b
-- checkBalance Nothing =
--   throwError err400 {errBody = "To check account balance, please provide account name"}

-- _checkBalance :: (MonadReader Config m, MonadIO m) => String -> m (Maybe Balance)
-- _checkBalance aname = do
--   a <- asks accounts
--   liftIO $ atomically $ fmap _balance . M.lookup aname <$> readTVar a

-- transfer :: AccountName -> AccountName -> Int -> Accounts -> IO ()
-- transfer from to amount accounts =
--   undefined
-- --TODO: Reformat this structure to easly transfer money !!
