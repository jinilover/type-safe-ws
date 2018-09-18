{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.Configurator
import Data.Configurator.Types
import ConfigTypes
import Control.Monad.Trans.Reader

loadAppConfig :: String -> IO AppConfig
loadAppConfig file = load [Required file] >>= runReaderT appReaderT
  where appReaderT = do
          appPort <- fieldReaderT "AppConfig.appPort"
          dbscriptsDir <- fieldReaderT "AppConfig.dbscriptsDir"
          db <- dbReaderT
          return $ AppConfig appPort dbscriptsDir db
        dbReaderT = do
          dbHost <- fieldReaderT "AppConfig.DbConfig.dbHost"
          dbName <- fieldReaderT "AppConfig.DbConfig.dbName"
          user <- fieldReaderT "AppConfig.DbConfig.user"
          password <- fieldReaderT "AppConfig.DbConfig.password"
          dbPort <- fieldReaderT "AppConfig.DbConfig.dbPort"
          return $ DbConfig dbHost dbName user password dbPort
        fieldReaderT name = ReaderT (`require` name)
