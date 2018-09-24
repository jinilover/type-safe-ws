{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Servant
import Network.Wai.Handler.Warp
import Data.Time.Calendar
import Database.PostgreSQL.Simple
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import GHC.IO.Exception
import Data.Pool

import TypeSafeWS.ApiTypes
import TypeSafeWS.Apis
import qualified TypeSafeWS.DbServices as Db
import TypeSafeWS.DataTypes
import TypeSafeWS.Config
import TypeSafeWS.ConfigTypes

main :: IO ()
main = do
  AppConfig{..} <- loadAppConfig
  pool <- Db.initConnPool dbConfig
  withResource pool (`Db.migrateDb` dbscriptsDir)
  run appPort $ app pool

server :: Pool Connection -> Server RestAPI
server pool = let withConnPool = withResource pool in
    return "Welcome to microservice in pure FP"
    :<|> withConnPool . flip sortUsers
    :<|> withConnPool . flip addUser
    :<|> withConnPool . flip deleteUser
    :<|> getServiceInfo

serviceApi :: Proxy RestAPI
serviceApi = Proxy

app :: Pool Connection -> Application
app = serve serviceApi . server
