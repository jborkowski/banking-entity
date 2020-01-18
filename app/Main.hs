{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
module Main where

-- import GHC.Generics
-- import GHC.TypeLits
import Network.Wai.Handler.Warp
import Servant
import Lib

type UserAPI = "users" :> Get '[JSON] [User]

users :: [User]
users =
    [ User { _firstName = "John", _lastName = "Doe", _email = "john.doe@exampl.com" } ]

server :: Server UserAPI
server = return users

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Application
app = serve userAPI server

main :: IO ()
main = run 8081 app



