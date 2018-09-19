{-# LANGUAGE DeriveGeneric #-}

module TypeSafeWS.DataTypes where

import Data.Time.Calendar
import GHC.Generics

data User = User {
  name :: String,
  age :: Int,
  email :: String,
  registrationDate :: String
} deriving (Show, Generic)
