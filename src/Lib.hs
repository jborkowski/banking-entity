{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Control.Lens
import Data.Text as T hiding (drop, toLower) 
import Data.Char (toLower)
import Models

import qualified Data.Map as M
import Control.Concurrent.STM

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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

checkBalance :: String
             -> TVar (M.Map String Account)
             -> STM (Maybe Int)
checkBalance email bank =
  do s <- readTVar bank
     return (fmap (\a -> _balance a) (M.lookup email s))

transfer :: Text -> Text -> Int
transfer = undefined
