{-# LANGUAGE RecordWildCards #-}

module Main where

import Servant
import Network.Wai.Handler.Warp
import Data.Time.Calendar
import Control.Monad.IO.Class

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
  addUser $ User "Isaac Newton" 372 "isaac@newton.co.uk" "1683-03-01"
  addUser $ User "Albert Einstein" 136 "ae@mc2.org"      "1905-12-01"
  run appPort app

server :: Server UserAPI
server = \sort -> liftIO $ sortBy sort <$> listAllUsers

userApi :: Proxy UserAPI
userApi = Proxy

app :: Application
app = serve userApi server
