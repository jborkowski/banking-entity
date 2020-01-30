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

newtype Name
  = Name
      { getName :: String
      }
  deriving (Generic, Show)

makeLenses ''Name

deriveJSON defaultOptions ''Name

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
        _balance :: Int
      }
  deriving (Generic, Eq, Show)

makeLenses ''Account

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Account

emptyAccount :: User -> Account
emptyAccount user = Account {_accountName = (_email user), _user = user, _balance = 0}
