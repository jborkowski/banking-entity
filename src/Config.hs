{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM (TVar)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks)
import qualified Data.Map as M (Map)
import Models (Accounts)
import Servant.Server.Internal (ServerError)

newtype AppT m a
  = AppT
      { runApp :: ReaderT Config (ExceptT ServerError m) a
      }
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadError ServerError, MonadIO)

type App = AppT IO

newtype Config = Config {accounts :: MVar Accounts}
