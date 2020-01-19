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
    deriving (Generic, Show)

makeLenses ''User
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''User

data Account =
    Account { _accountName :: String
            , _user :: User
            , _balance :: Int
            }
    deriving (Generic, Show)

makeLenses ''Account
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Account



-- updateMoneyAndStockStm :: Eq a => a -> Integer
--                        -> TVar Integer -> TVar [(a,Integer)] -> STM ()
-- updateMoneyAndStockStm product price money stock =
--   do s <- readTVar stock
--      let Just productNo = lookup product s
--      if productNo > 0
--        then do m <- readTVar money
--                let newS = map (\(k,v) -> if k == product
--                                             then (k,v-1)
--                                             else (k,v)) s
--                writeTVar money (m + price) >> writeTVar stock newS
--        else return ()

-- Add STM with state

exampleAccount = User { _firstName = "Joey", _lastName = "Tribbiani", _email = "joey.tribbiani@mov.com"}

newAccountName :: Fold User String
newAccountName =
    folding (\s -> s ^.. firstName . to (fmap toLower)
                <> s ^.. lastName . to (fmap toLower))

createAccount :: User
              -> TVar (M.Map String Account)
              -> STM ()
createAccount user state =
  do s <- readTVar state
     let key = user ^.. newAccountName . folded
         maybeAccout = M.lookup accountName s
           if isNothing maybeAccount then
             let account :: Account
                 account = Account { _accountName = accountName, _user = user, _balance = 0 }
                 newAccountMap = M.Map String Account
                 newAccountMap = M.insert accountName account s
              writeTVar state account
           else return ()               

deposit :: Text -> Int
deposit = undefined

withdraw :: Text -> Int
withdraw = undefined

checkBalance :: Text -> Int
checkBalance = undefined

transfer :: Text -> Text -> Int
transfer = undefined
