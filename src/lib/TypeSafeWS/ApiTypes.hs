{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeSafeWS.ApiTypes where

import Data.Int (Int64)
import Data.Text
import Data.Time (Day)
import Servant.API

import TypeSafeWS.DataTypes

type UserAPI = "users" :> QueryParam "sortBy" SortBy :> Get '[JSON] [User]
          :<|> "users" :> ReqBody '[JSON] User :> Post '[PlainText] String

data SortBy = Age | Name

instance FromHttpApiData SortBy where
  parseQueryParam value
    | toLower value == "age" = Right Age
    | toLower value == "name" = Right Name
    | otherwise = Left "Invalid sortby value"
