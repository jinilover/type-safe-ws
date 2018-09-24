{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TypeSafeWS.DbServices where

import Control.Exception.Base
import Data.List
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
      url = BS8.pack $
            "host='" ++ dbHost ++
            "' dbname='" ++ dbName ++
            "' user='" ++ user ++
            "' password='" ++ password ++
            "' port=" ++
            show dbPort in
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

addUser :: User -> IO (Either AddUserError String)
addUser user@User{..} = getDbConn >>= (`insertUser` (parseDay $ BS8.pack registrationDate))
  where insertUser :: Connection -> Either String Day -> IO (Either AddUserError String)
        insertUser _ (Left parseErr) = return $ Left $ InvalidDate $ parseErr ++ " from date " ++ registrationDate
        insertUser conn (Right date) =
          let addUserIO = const (Right $ "User " ++ name ++ " created")
                          <$> execute conn "INSERT INTO users VALUES (?, ?, ?, ?)" (name, age, email, date) in
          catch addUserIO handleError
        handleError :: SomeException -> IO (Either AddUserError String)
        handleError (SomeException e) =
          let errMsg = displayException e in
          if "duplicate key value" `isInfixOf` errMsg
              then return $ Left $ UserAlreadyExisted errMsg
              else throwIO e

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
