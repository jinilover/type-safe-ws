module TypeSafeWS.Config where

import Data.Configurator
import Data.Configurator.Types
import Control.Monad.Trans.Reader
import Protolude
import Prelude (String)

import TypeSafeWS.ConfigTypes
import TypeSafeWS.Utils

loadAppConfig :: [String] -> IO AppConfig
loadAppConfig = loadWholeConfig . resourceFolder
  where loadWholeConfig folder =
          load [Required $ folder ++ "appl.cfg"] >>= runReaderT appReaderT
        appReaderT = AppConfig <$> fieldReaderT "AppConfig.appPort"
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
