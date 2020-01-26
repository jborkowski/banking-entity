{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api (app) where

import           Control.Monad.Reader (runReaderT)
import           Servant              ((:<|>) ((:<|>)),
                                       Proxy (Proxy), Raw, Server,
                                       serve, serveDirectoryFileServer)
import           Servant.Server

import           Api.Account          (AccountAPI, accountServer, accountApi)
import           Config               (AppT (..), Config (..))

accountApp :: Config -> Application
accountApp cfg = serve accountApi (appToServer cfg)

appToServer :: Config -> Server AccountAPI
appToServer cfg = hoistServer accountApi (convertApp cfg) accountServer

convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

type AppAPI = AccountAPI --  :<|> OperationAPI

appApi :: Proxy AppAPI
appApi = Proxy

app :: Config -> Application
app cfg =
    serve appApi (appToServer cfg)
