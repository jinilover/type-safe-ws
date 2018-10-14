module Main where

import Servant
import Network.Wai.Handler.Warp
import Database.PostgreSQL.Simple
import Data.Pool

import qualified TypeSafeWS.DbServices as DbServices
import TypeSafeWS.ApiTypes
import TypeSafeWS.DataTypes
import TypeSafeWS.Config
import TypeSafeWS.ConfigTypes
import TypeSafeWS.Server

main :: IO ()
main = do
  AppConfig{..} <- loadAppConfig
  pool <- DbServices.initConnPool dbConfig
  withResource pool (`DbServices.migrateDb` dbscriptsDir)
  run appPort $ app pool

app :: Pool Connection -> Application
app = serve serviceApi . server . DbServices.createDb
