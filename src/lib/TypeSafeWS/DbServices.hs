{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TypeSafeWS.DbServices where

import Data.Int (Int64)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import Data.ByteString.Char8

import TypeSafeWS.ConfigTypes
import TypeSafeWS.DataTypes

getDbConn :: DbConfig -> IO Connection
getDbConn (DbConfig host name user passwd port) =
  let url = pack $ "host='" ++ host ++ "' dbname='" ++ name ++ "' user='" ++ user ++ "' password='" ++ passwd ++ "' port=" ++ show port in
      connectPostgreSQL url

migrateDb :: String -> Connection -> IO ()
migrateDb dir conn = do
  initResult <- withTransaction conn $ runMigration $
    MigrationContext MigrationInitialization True conn
  print $ "MigrationInitialization: " ++ show initResult
  migrateResult <- withTransaction conn $ runMigration $
    MigrationContext (MigrationDirectory dir) True conn
  print $ "Migration result: " ++ show migrateResult

addUser :: Connection -> User -> IO Int64
addUser conn User{..} = execute conn "INSERT INTO users VALUES (?, ?, ?, ?)" (name, age, email, registrationDate)
