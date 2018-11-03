module Main where

import Servant
import Network.Wai.Handler.Warp
import Database.PostgreSQL.Simple
import Data.Pool
import Protolude

import qualified TypeSafeWS.DbServices as DbServices
import TypeSafeWS.ApiTypes
import TypeSafeWS.DataTypes
import TypeSafeWS.Config
import TypeSafeWS.ConfigTypes
import TypeSafeWS.Server

main :: IO ()
main = do
  args <- getArgs
  AppConfig{..} <- loadAppConfig args
  pool <- DbServices.initConnPool dbConfig
  withResource pool (`DbServices.migrateDb` args)
  run appPort $ app pool

app :: Pool Connection -> Application
app = serve serviceApi . server . DbServices.createDb
