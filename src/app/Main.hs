{-# LANGUAGE RecordWildCards #-}

module Main where

import Servant
import Network.Wai.Handler.Warp
import Data.Time.Calendar
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import GHC.IO.Exception

import TypeSafeWS.DbServices
import TypeSafeWS.Apis
import TypeSafeWS.ApiTypes
import TypeSafeWS.DataTypes
import TypeSafeWS.Config
import TypeSafeWS.ConfigTypes

main :: IO ()
main = do
  AppConfig{..} <- loadAppConfig
  migrateDb dbscriptsDir
  run appPort app

server :: Server UserAPI
server = (\sort -> liftIO $ sortBy sort <$> listAllUsers)
    :<|> liftIO . addUser

userApi :: Proxy UserAPI
userApi = Proxy

app :: Application
app = serve userApi server
