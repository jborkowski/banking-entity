

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
module Lib (someFunc) where

import Control.Lens
import Data.Text
import GHC.Generics
import Data.Aeson

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data User =
    User { _firstName :: Text
         , _lastName :: Text
         , _email :: Text
         }
    deriving (Generic, Show)

makeLenses ''User
instance ToJSON User

data Account =
    Account { _accountId :: Int
            , _user :: User
            , balance :: Int
            }
    deriving (Generic, Show)

makeLenses ''Account
instance ToJSON Account




