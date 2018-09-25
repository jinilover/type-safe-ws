{-# LANGUAGE DeriveGeneric #-}

module TypeSafeWS.DataTypes where

import Data.Time.Calendar
import Data.Aeson.Types
import GHC.Generics

data User = User {
  name :: String,
  age :: Int,
  email :: String,
  registrationDate :: String
} deriving (Show, Generic)

instance ToJSON User
instance FromJSON User

data AddUserError = InvalidDate { msg :: String }
                  | UserAlreadyExisted { msg :: String }
