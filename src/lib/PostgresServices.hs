{-# LANGUAGE OverloadedStrings #-}
module PostgresServices where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import qualified Data.ByteString.Char8 as BS8
import ConfigTypes

getDbConn :: DbConfig -> IO Connection
getDbConn (DbConfig host name user passwd port) =
    let url = BS8.pack $ "host='" ++ host ++ "' dbname='" ++ name ++ "' user='" ++ user ++ "' password='" ++ passwd ++ "' port=" ++ show port in
        connectPostgreSQL url

migrateDb :: AppConfig -> IO ()
migrateDb appConfig = do
  conn <- getDbConn dbCfg
  initResult <- withTransaction conn $ runMigration $
    MigrationContext MigrationInitialization True conn
  print $ "MigrationInitialization: " ++ show initResult
  migrateResult <- withTransaction conn $ runMigration $
    MigrationContext (MigrationDirectory dir) True conn
  print $ "Migration result: " ++ show migrateResult
  where dbCfg = dbConfig appConfig
        dir = dbscriptsDir appConfig
