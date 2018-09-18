{-# LANGUAGE DeriveGeneric #-}

module DataTypes where

import Data.Time.Calendar
import GHC.Generics

-- TODO to be replaced by db access logic
users :: [User]
users = [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
        , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
        ]

data User = User {
  name :: String,
  age :: Int,
  email :: String,
  registrationDate :: Day
} deriving Generic
