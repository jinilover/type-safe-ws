{-# LANGUAGE RecordWildCards #-}

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

server :: Server UserAPI
server = sortUsers
    :<|> addUser
    :<|> deleteUser

userApi :: Proxy UserAPI
userApi = Proxy

app :: Application
app = serve userApi server
