{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Servant
import Network.Wai.Handler.Warp
import Data.Time.Calendar
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import GHC.IO.Exception

import TypeSafeWS.Apis
import TypeSafeWS.ApiTypes
import qualified TypeSafeWS.DbServices as Db
import TypeSafeWS.DataTypes
import TypeSafeWS.Config
import TypeSafeWS.ConfigTypes

main :: IO ()
main = do
  AppConfig{..} <- loadAppConfig
  Db.migrateDb dbscriptsDir
  run appPort app

server :: Server RestAPI
server = return "Welcome to microservice in pure FP"
    :<|> sortUsers
    :<|> addUser
    :<|> deleteUser
    :<|> getServiceInfo

serviceApi :: Proxy RestAPI
serviceApi = Proxy

app :: Application
app = serve serviceApi server
