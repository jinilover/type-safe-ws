module TypeSafeWS.DbServices where

import Control.Exception.Base hiding (throwIO)
import Data.List hiding (map)
import Data.Time.Calendar
import Data.Int (Int64)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time
import Database.PostgreSQL.Simple.Migration
import Data.Either
import Data.Pool
import Protolude
import Prelude (String)
import qualified Data.ByteString.Char8 as BS8

import TypeSafeWS.ConfigTypes
import TypeSafeWS.DataTypes
import TypeSafeWS.Config

initConnPool :: DbConfig -> IO (Pool Connection)
initConnPool DbConfig{..} =
  let url = BS8.pack $
            "host='" ++ dbHost ++
            "' dbname='" ++ dbName ++
            "' user='" ++ user ++
            "' password='" ++ password ++
            "' port=" ++
            show dbPort in
      createPool (connectPostgreSQL url) close noOfStripes (realToFrac idleTime) stripeSize

createDb :: Pool Connection -> Db
createDb pool = let withConnPool = withResource pool in
  Db {
    _addUser = withConnPool . flip addUser
  , _listAllUsers = withConnPool listAllUsers
  , _deleteUser = withConnPool . flip deleteUser
  }

migrateDb :: Connection -> String -> IO ()
migrateDb conn dir = do
  initResult <- withTransaction conn . runMigration $
    MigrationContext MigrationInitialization True conn
  print $ "MigrationInitialization: " ++ show initResult
  migrateResult <- withTransaction conn . runMigration $
    MigrationContext (MigrationDirectory dir) True conn
  print $ "Migration result: " ++ show migrateResult

addUser :: Connection -> User -> IO (Either AddUserError String)
addUser conn user@User{..} = insertUser (parseDay $ BS8.pack registrationDate)
  where insertUser :: Either String Day -> IO (Either AddUserError String)
        insertUser (Left parsedErr) = return . Left . InvalidDate $ parsedErr ++ " from date " ++ registrationDate
        insertUser (Right date) =
          let addUserIO = const (Right $ "User " ++ name ++ " created")
                          <$> execute conn "INSERT INTO users VALUES (?, ?, ?, ?)" (name, age, email, date) in
          catch addUserIO handleError
        handleError :: SomeException -> IO (Either AddUserError String)
        handleError (SomeException e) =
          let errMsg = displayException e in
          if "duplicate key value" `isInfixOf` errMsg
              then return . Left $ UserAlreadyExisted errMsg
              else throwIO e

listAllUsers :: Connection -> IO [User]
listAllUsers conn = map toUser <$> (query_ conn sql :: IO [(String, Int, String, Day)])
  where toUser (name, age, email, date) = User name age email (show date)
        sql = "SELECT user_name, age, email, registration_date FROM users"

deleteUser :: Connection -> String -> IO Int64
deleteUser conn = execute conn "DELETE FROM users WHERE user_name = ?" . Only
