

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Lib where

import Control.Lens
import Data.Text hiding (drop)
import Data.Aeson
import Data.Aeson.TH
--import Data.Aeson.Types
import GHC.Generics

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data User =
    User { _firstName :: Text
         , _lastName :: Text
         , _email :: Text
         }
    deriving (Generic, Show)

makeLenses ''User
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''User

data Account =
    Account { _accountId :: Int
            , _user :: User
            , balance :: Int
            }
    deriving (Generic, Show)

makeLenses ''Account
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Account




