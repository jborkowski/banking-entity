{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( app,
  )
where

import Api.Account (AccountAPI, accountApi, accountServer)
import Api.Operations (OperationsAPI, operationsApi, operationsServer)
import Config (AppT (..), Config (..))
import Control.Monad.Reader (runReaderT)
import Servant
  ( (:<|>) ((:<|>)),
    Proxy (Proxy),
    Server,
    serve,
  )
import Servant.Server

accountApp :: Config -> Server AccountAPI
accountApp cfg = hoistServer accountApi (convertApp cfg) accountServer

operationsApp :: Config -> Server OperationsAPI
operationsApp cfg = hoistServer operationsApi (convertApp cfg) operationsServer

appToServer :: Config -> Server AppAPI
appToServer cfg = accountApp cfg :<|> operationsApp cfg

convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

type AppAPI = AccountAPI :<|> OperationsAPI

appApi :: Proxy AppAPI
appApi = Proxy

app :: Config -> Application
app cfg =
  serve appApi (appToServer cfg)
