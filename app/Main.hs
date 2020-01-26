{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
module Main where

import Network.Wai.Handler.Warp (run)
import Servant
import qualified Data.Map as M
import Control.Concurrent.STM (readTVar, writeTVar, TVar, newTVar, atomically)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Exception (throwIO)
import Models
import Lib

newtype State = State { accounts :: TVar (M.Map String Account) }

type AppM = ReaderT State Handler

type GetAccount = QueryParam "accountName" String :> Get '[JSON] (Maybe Account)
type AddAccount = ReqBody '[JSON] User :> PostCreated '[JSON] ()
type AccountAPI = "account" :> (GetAccount :<|> AddAccount)

api :: Proxy AccountAPI
api = Proxy

server :: ServerT AccountAPI AppM
server = getAccount :<|> addAccount

getAccount :: Maybe String -> AppM (Maybe Account)
getAccount (Just aname) =  do
  State { accounts = a } <- ask
  liftIO $ atomically $ M.lookup aname <$> readTVar a
getAccount Nothing =
  return Nothing
--  liftIO $ throwIO err400 { errBody = "Missing 'accountName' parameter" }

-- Add Error handling 
addAccount :: User -> AppM ()
addAccount newUser= do
  State { accounts = a } <- ask
  liftIO $ atomically $ readTVar a >>= writeTVar a . M.insert (_email newUser) (emptyAccount newUser)

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

app :: State -> Application
app s = serve api $ hoistServer api (nt s) server

main :: IO ()
main = do
  let port = 8081
  initState <- atomically $ newTVar M.empty
  run port $ app $ State initState

