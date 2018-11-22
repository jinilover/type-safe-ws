{-# LANGUAGE DeriveGeneric #-}

module TypeSafeWS.DataTypes where

import Data.Time.Calendar
import Data.Aeson.Types
import Data.Int (Int64)
import GHC.Generics
import Protolude
import Prelude (String)

data ServiceInfo = ServiceInfo {
  gitHash :: String
, gitBranch :: String
, gitCommitDate :: String
, gitCommitMessage :: String
} deriving Generic

instance ToJSON ServiceInfo

data User = User {
  name :: String
, age :: Int
, email :: String
, registrationDate :: String
} deriving (Show, Generic, Eq)

instance ToJSON User
instance FromJSON User

data AddUserError = InvalidDate { msg :: String }
                  | UserAlreadyExisted { msg :: String }
                  deriving (Show, Eq)

data Db = Db {
  _listAllUsers :: IO [User]
, _addUser :: User -> IO (Either AddUserError String)
, _deleteUser :: String -> IO Int64
}
