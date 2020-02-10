{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

-- import Control.Concurrent.STM
-- import Control.Lens
-- import Data.Char (toLower)
-- import qualified Data.Map as M
-- import Data.Text as T hiding (drop, toLower)
-- import Models

-- newAccountName :: Fold User String
-- newAccountName =
--   folding
--     ( \s ->
--         s ^.. firstName . to (fmap toLower)
--           <> s ^.. lastName . to (fmap toLower)
--     )

-- createAccount ::
--   User ->
--   TVar (M.Map AccountName Account) ->
--   STM ()
-- createAccount user bank =
--   do
--     s <- readTVar bank
--     let email = _email user
--     if (/= Nothing) (M.lookup email s)
--       then writeTVar bank (M.insert email (emptyAccount user) s)
--       else return ()

-- checkBalance ::
--   String ->
--   TVar (M.Map String Account) ->
--   STM (Maybe Balance)
-- checkBalance email bank =
--   do
--     s <- readTVar bank
--     return (fmap (\a -> _balance a) (M.lookup email s))
