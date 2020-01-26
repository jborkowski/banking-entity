{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Account where

import Control.Concurrent.STM (readTVar, writeTVar, TVar, newTVar, atomically)
--import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.Except        (MonadIO, liftIO)
import           Control.Monad.Logger        (logDebugNS)
import           Servant

import           Config                      (AppT (..))
import           Data.IORef                  (readIORef)
import           Data.Text                   (Text)
import           Models                      (Account, User)


type GetAccount = QueryParam "accountName" String :> Get '[JSON] Account
type AddAccount = ReqBody '[JSON] User :> PostCreated '[JSON] ()
type AccountAPI = "account" :> (GetAccount :<|> AddAccount)

-- type Metrics = "metrics" :> Get '[JSON] (HashMap Text Int64)

accountApi :: Proxy AccountAPI
accountApi = Proxy

accountServer :: MonadIO m => ServerT AccountAPI (AppT m)
accountServer = getAccount :<|> addAccount


-- FIXME: Add correct...
addAccount :: MonadIO m => User -> AppT m () 
addAccount user = do
  --State { accounts = a } <- ask
  --liftIO $ atomically $ readTVar a >>= writeTVar a . M.insert (_email newUser) (emptyAccount newUser)
  -- logDebugNS "account" "creating a account"
  --State { accounts = a } <- ask
  undefined

getAccount :: MonadIO m => Maybe String -> AppT m Account
getAccount = undefined

-- getAccount :: Maybe String -> AppM (Maybe Account)
-- getAccount (Just aname) =  do
--   State { accounts = a } <- ask
--   liftIO $ atomically $ M.lookup aname <$> readTVar a
-- getAccount Nothing =
--   return Nothing
-- --  liftIO $ throwIO err400 { errBody = "Missing 'accountName' parameter" }

-- -- Add Error handling 
-- addAccount :: User -> AppM ()
-- addAccount newUser= do
--   State { accounts = a } <- ask
--   liftIO $ atomically $ readTVar a >>= writeTVar a . M.insert (_email newUser) (emptyAccount newUser)

-- -- | Returns a user by name or throws a 404 error.
-- singleUser :: MonadIO m => Text -> AppT m (Entity User)
-- singleUser str = do
--     increment "singleUser"
--     logDebugNS "web" "singleUser"
--     maybeUser <- runDb (selectFirst [Md.UserName ==. str] [])
--     case maybeUser of
--          Nothing ->
--             throwError err404
--          Just person ->
--             return person

-- | Creates a user in the database.
-- createUser :: MonadIO m => User -> AppT m Int64
-- createUser p = do
--     increment "createUser"
--     logDebugNS "web" "creating a user"
--     newUser <- runDb (insert (User (userName p) (userEmail p)))
--     return $ fromSqlKey newUser

-- | Return wai metrics as JSON
-- waiMetrics :: MonadIO m => AppT m (HashMap Text Int64)
-- waiMetrics = do
--     increment "metrics"
--     logDebugNS "web" "metrics"
-- --    metr <- Metrics.getMetrics
--    liftIO $ mapM Counter.read =<< readIORef (metr ^. metricsCounters)


  
