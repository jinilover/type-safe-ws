{-# LANGUAGE RecordWildCards #-}

module Main where

import Servant
import Network.Wai.Handler.Warp

import TypeSafeWS.DbServices
import TypeSafeWS.Services
import TypeSafeWS.ApiTypes
import TypeSafeWS.DataTypes
import TypeSafeWS.Config
import TypeSafeWS.ConfigTypes

main :: IO ()
main = do
  AppConfig{..} <- loadAppConfig "src/resources/appl.cfg"
  conn <- getDbConn dbConfig
  migrateDb dbscriptsDir conn
  run appPort app

server :: Server UserAPI
server = return . sortBy

userApi :: Proxy UserAPI
userApi = Proxy

app :: Application
app = serve userApi server
