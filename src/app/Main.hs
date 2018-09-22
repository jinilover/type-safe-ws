{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Servant
import Network.Wai.Handler.Warp
import Data.Time.Calendar
import Control.Exception.Base
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
  try $ Db.addUser User { name = "Isaac Newton"
                  , age = 372
                  , email = "isaac@newton.co.uk"
                  , registrationDate = "1683-03-01"}
  run appPort app

server :: Server ServiceAPI
server = return "Welcome to microservice in pure FP"
    :<|> sortUsers
    :<|> addUser
    :<|> deleteUser

serviceApi :: Proxy ServiceAPI
serviceApi = Proxy

app :: Application
app = serve serviceApi server
