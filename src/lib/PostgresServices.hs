{-# LANGUAGE OverloadedStrings #-}
module PostgresServices where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration

getDbConn :: IO Connection
getDbConn =
    let url = "host='localhost' dbname='postgres' user='postgres' password='password' port=5445" in
        connectPostgreSQL url

migrateDb :: String -> IO ()
migrateDb dir = do
  conn <- getDbConn
  initResult <- withTransaction conn $ runMigration $
    MigrationContext MigrationInitialization True conn
  print $ "MigrationInitialization: " ++ show initResult
  migrateResult <- withTransaction conn $ runMigration $
    MigrationContext (MigrationDirectory dir) True conn
  print $ "Migration result: " ++ show migrateResult
