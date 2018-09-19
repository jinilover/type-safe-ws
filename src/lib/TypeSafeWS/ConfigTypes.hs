module TypeSafeWS.ConfigTypes where

data AppConfig = AppConfig { appPort :: Int
                           , dbscriptsDir :: String
                           , dbConfig :: DbConfig } deriving (Show, Eq)

data DbConfig = DbConfig { dbHost :: String
                         , dbName :: String
                         , user :: String
                         , password :: String
                         , dbPort :: Int } deriving (Show, Eq)