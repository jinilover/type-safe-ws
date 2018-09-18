module Main where

import Servant

import Services
import ApiTypes
import DataTypes
import Config
import ConfigTypes
import PostgresServices
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  migrateDb "src/resources/dbscripts"
  loadAppConfig "src/resources/appl.cfg" >>= ((`run` app) . port)

server :: Server UserAPI
server = return . sortBy

userApi :: Proxy UserAPI
userApi = Proxy

app :: Application
app = serve userApi server
