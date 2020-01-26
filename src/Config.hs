{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Config where

import Control.Monad.Reader       (ReaderT, MonadReader)
import Control.Concurrent.STM     (TVar)
import Control.Monad.IO.Class     (MonadIO)
import Control.Monad.Except       (ExceptT, MonadError)
import qualified Data.Map as M    (Map)
import Servant.Server.Internal    (ServerError)
import Models                     (Account)

newtype AppT m a = AppT
    { runApp :: ReaderT Config (ExceptT ServerError m) a
    } deriving ( Functor, Applicative, Monad, MonadReader Config, MonadError ServerError, MonadIO )

type App = AppT IO

newtype Config = Config { accounts :: TVar (M.Map String Account) }

