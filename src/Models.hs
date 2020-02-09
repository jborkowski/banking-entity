{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Models where

import Control.Concurrent.STM (TVar)
import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import GHC.Generics

type Balance = Int

type AccountName = String

data User
  = User
      { _firstName :: String,
        _lastName :: String,
        _email :: String
      }
  deriving (Generic, Eq, Show)

makeLenses ''User

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''User

data Account
  = Account
      { _name :: AccountName,
        _user :: User,
        _balance :: Balance
      }
  deriving (Generic, Eq, Show)

makeLenses ''Account

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Account

emptyAccount :: User -> Account
emptyAccount user = Account {_name = (_email user), _user = user, _balance = 0}

type Accounts = TVar (M.Map AccountName Account)

data OperationForm
  = OperationForm
      { _accountName :: AccountName,
        _ammount :: Int
      }
  deriving (Generic, Show)

makeLenses ''OperationForm

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''OperationForm

data TransferForm
  = TransferForm
      { _from :: AccountName,
        _to :: AccountName,
        _transferAmmount :: Int
      }

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''TransferForm
