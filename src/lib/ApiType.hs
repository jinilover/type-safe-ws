{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ApiType where

import Data.Text
import Data.Time (Day)
import Servant.API
import Data.Aeson.Types
import GHC.Generics

type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]

instance ToJSON User

data SortBy = Age | Name
instance FromHttpApiData SortBy where
  parseQueryParam value
    | value == "age" = Right Age
    | toLower value == "name" = Right Name
    | otherwise = Left "Invalid sortby value"

data User = User {
  name :: String,
  age :: Int,
  email :: String,
  registrationDate :: Day
} deriving Generic
