{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeSafeWS.ApiTypes where

import Data.Int (Int64)
import Data.Text
import Data.Time (Day)
import Servant.API

import TypeSafeWS.DataTypes
import TypeSafeWS.Git

type RestAPI = Get '[PlainText] Text
          :<|> "users" :> QueryParam "sortBy" SortBy :> Get '[JSON] [User]
          :<|> "users" :> ReqBody '[JSON] User :> Post '[PlainText] String
          :<|> "users" :> Capture "user_name" String :> Delete '[PlainText] String
          :<|> "service_info" :> Get '[JSON] GitInfo

data SortBy = Age | Name

instance FromHttpApiData SortBy where
  parseQueryParam value
    | toLower value == "age" = Right Age
    | toLower value == "name" = Right Name
    | otherwise = Left "Invalid sortby value"
