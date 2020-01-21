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
{-# LANGUAGE LambdaCase #-}

module Lib where

import Control.Lens
import Data.Text as T hiding (drop, toLower) 
import Data.Aeson
import Data.Aeson.TH
import Data.Char (toLower)

import qualified Data.Map as M
--import Data.Aeson.Types
import GHC.Generics hiding (to)
import Control.Concurrent.STM

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Name = Name
    { getName :: String
    } deriving (Generic, Show)
makeLenses ''Name
deriveJSON defaultOptions ''Name

data User =
    User { _firstName :: String
         , _lastName :: String
         , _email :: String
         }
    deriving (Generic, Eq, Show)

makeLenses ''User
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''User

data Account =
    Account { _accountName :: String
            , _user :: User
            , _balance :: Int
            }
    deriving (Generic, Eq, Show)

makeLenses ''Account
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Account

exampleAccount = User { _firstName = "Joey", _lastName = "Tribbiani", _email = "joey.tribbiani@mov.com"}

newAccountName :: Fold User String
newAccountName =
    folding (\s -> s ^.. firstName . to (fmap toLower)
                <> s ^.. lastName . to (fmap toLower))

emptyAccount :: User -> Account
emptyAccount user = Account { _accountName = (_email user), _user = user, _balance = 0 }

createAccount :: User
              -> TVar (M.Map String Account)
              -> STM ()
createAccount user bank =
  do s <- readTVar bank
     let email = _email user
     if (/=Nothing) (M.lookup email s) then
       writeTVar bank (M.insert email (emptyAccount user) s)
     else return ()

deposit :: Text -> Int
deposit = undefined

withdraw :: Text -> Int
withdraw = undefined

checkBalance :: Text -> Int
checkBalance = undefined

transfer :: Text -> Text -> Int
transfer = undefined
