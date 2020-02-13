{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Api (app)
import Config (Config (..))
import Control.Concurrent.MVar (newMVar)
import qualified Data.Map as M (empty)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  let port = 8081
  initialize <- newMVar M.empty
  run port $ app $ Config initialize
