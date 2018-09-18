{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.Configurator
import Data.Configurator.Types
import ConfigTypes
import Control.Monad.Trans.Reader

loadAppConfig :: String -> IO AppConfig
loadAppConfig file = load [Required file] >>= runReaderT appReaderT
  where appReaderT = AppConfig <$> fieldReaderT "AppConfig.appPort"
                               <*> fieldReaderT "AppConfig.dbscriptsDir"
                               <*> dbReaderT
        dbReaderT = DbConfig <$> fieldReaderT "AppConfig.DbConfig.dbHost"
                             <*> fieldReaderT "AppConfig.DbConfig.dbName"
                             <*> fieldReaderT "AppConfig.DbConfig.user"
                             <*> fieldReaderT "AppConfig.DbConfig.password"
                             <*> fieldReaderT "AppConfig.DbConfig.dbPort"
        fieldReaderT name = ReaderT (`require` name)
