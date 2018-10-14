module TypeSafeWS.Config where

import Data.Configurator
import Data.Configurator.Types
import Control.Monad.Trans.Reader

import TypeSafeWS.ConfigTypes

loadAppConfig :: IO AppConfig
loadAppConfig = load [Required "src/resources/appl.cfg"] >>= runReaderT appReaderT
  where appReaderT = AppConfig <$> fieldReaderT "AppConfig.appPort"
                               <*> fieldReaderT "AppConfig.dbscriptsDir"
                               <*> dbReaderT
        dbReaderT = DbConfig <$> fieldReaderT "AppConfig.DbConfig.dbHost"
                             <*> fieldReaderT "AppConfig.DbConfig.dbName"
                             <*> fieldReaderT "AppConfig.DbConfig.user"
                             <*> fieldReaderT "AppConfig.DbConfig.password"
                             <*> fieldReaderT "AppConfig.DbConfig.dbPort"
                             <*> fieldReaderT "AppConfig.DbConfig.noOfStripes"
                             <*> fieldReaderT "AppConfig.DbConfig.idleTime"
                             <*> fieldReaderT "AppConfig.DbConfig.stripeSize"
        fieldReaderT name = ReaderT (`require` name)
