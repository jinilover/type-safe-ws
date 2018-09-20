{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TypeSafeWS.DbServices where

import Data.Time.Calendar
import Data.Int (Int64)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time
import Database.PostgreSQL.Simple.Migration
import Data.Either
import qualified Data.ByteString.Char8 as BS8

import TypeSafeWS.ConfigTypes
import TypeSafeWS.DataTypes
import TypeSafeWS.Config

getDbConn :: IO Connection
getDbConn = do
  AppConfig{..} <- loadAppConfig
  let DbConfig{..} = dbConfig
      url = BS8.pack $ "host='" ++ dbHost ++ "' dbname='" ++ dbName ++ "' user='" ++ user ++ "' password='" ++ password ++ "' port=" ++ show dbPort in
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

addUser :: User -> IO String
addUser user@User{..} = do
  conn <- getDbConn
  either (fail . failedCause) (insert conn) (parseDay $ BS8.pack registrationDate)
  return $ "User " ++ name ++ " created"
  where insert conn date = execute conn "INSERT INTO users VALUES (?, ?, ?, ?)" (name, age, email, date)
        failedCause = (++ " from " ++ show user)

listAllUsers :: IO [User]
listAllUsers = do
  conn <- getDbConn
  tuples <- query_ conn "SELECT user_name, age, email, registration_date FROM users" :: IO [(String, Int, String, Day)]
  return $ toUser <$> tuples
  where toUser (name, age, email, date) = User name age email (show date)

deleteUser :: String -> IO Int64
deleteUser user_name =
  getDbConn >>= (\conn ->
    execute conn "DELETE FROM users WHERE user_name = ?" $ Only user_name)
