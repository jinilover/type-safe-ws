{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.Configurator
import Data.Configurator.Types
import ConfigTypes

loadAppConfig :: String -> IO AppConfig
loadAppConfig file = fmap AppConfig $ load [Required file] >>= (`require` "Config.AppConfig.port")
