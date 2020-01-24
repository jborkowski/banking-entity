{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
module Main where

-- import GHC.Generics
-- import GHC.TypeLits
import Network.Wai.Handler.Warp
import Servant
import qualified Data.Map as M
import Lib
import Control.Concurrent.STM

type UserAPI = "users" :> Get '[JSON] [User]
           :<|> "johndoe" :> Get '[JSON] User
           :<|> "evacassidy" :> Get '[JSON] User

johndoe :: User
johndoe = User { _firstName = "John", _lastName = "Doe", _email = "john.doe@exampl.com" }

evacassidy :: User
evacassidy = User { _firstName = "Eva", _lastName = "Cassidy", _email = "evac@mus.gov" }

users :: [User]
users =
    [ johndoe
    , evacassidy
    ]

server :: Server UserAPI
server = return users
     :<|> return johndoe
     :<|> return evacassidy

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Application
app = serve userAPI server

main :: IO ()
-- main = run 8081 app
main =
  do bank <- newTVarIO (M.empty)
     run 8081 app


