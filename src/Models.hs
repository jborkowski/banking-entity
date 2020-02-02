{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Models where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import GHC.Generics

type Balance = Int

--makeLenses ''Balance

emptyBalance :: Balance
emptyBalance = 0

newtype Name
  = Name
      { getName :: String
      }
  deriving (Generic, Show)

makeLenses ''Name

-- deriveJSON defaultOptions ''Name

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
      { _accountName :: String,
        _user :: User,
        _balance :: Balance
      }
  deriving (Generic, Eq, Show)

makeLenses ''Account

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Account

emptyAccount :: User -> Account
emptyAccount user = Account {_accountName = (_email user), _user = user, _balance = emptyBalance}

data DepositForm
  = DepositForm
      { _aName :: String,
        _ammount :: Int
      }
  deriving (Generic, Show)

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''DepositForm
