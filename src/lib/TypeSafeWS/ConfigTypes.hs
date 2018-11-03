module TypeSafeWS.ConfigTypes where

import Protolude
import Prelude (String)

data AppConfig = AppConfig { appPort :: Int
                           , dbConfig :: DbConfig
                           } deriving (Show, Eq)

data DbConfig = DbConfig { dbHost :: String
                         , dbName :: String
                         , user :: String
                         , password :: String
                         , dbPort :: Int
                         , noOfStripes :: Int
                         , idleTime :: Int
                         , stripeSize :: Int
                         } deriving (Show, Eq)
