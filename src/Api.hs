{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( app,
  )
where

import Api.Account (AccountAPI, accountApi, accountServer)
import Config (AppT (..), Config (..))
import Control.Monad.Reader (runReaderT)
import Servant
  ( (:<|>) ((:<|>)),
    Proxy (Proxy),
    Raw,
    Server,
    serve,
    serveDirectoryFileServer,
  )
import Servant.Server

-- accountApp :: Config -> Application
-- accountApp cfg = serve accountApi (appToServer cfg)

appToServer :: Config -> Server AccountAPI
appToServer cfg = hoistServer accountApi (convertApp cfg) accountServer

convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

-- Add more features
type AppAPI = AccountAPI

appApi :: Proxy AppAPI
appApi = Proxy

app :: Config -> Application
app cfg =
  serve appApi (appToServer cfg)
