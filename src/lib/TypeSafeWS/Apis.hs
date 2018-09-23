{-# LANGUAGE FlexibleContexts #-}
module TypeSafeWS.Apis where

import GHC.Exts
import Servant
import Control.Monad.IO.Class
import Control.Exception.Base
import System.IO.Error
import Data.Int (Int64)

import TypeSafeWS.ApiTypes
import TypeSafeWS.DataTypes
import qualified TypeSafeWS.DbServices as Db
import qualified Data.ByteString.Char8 as BS8

sortUsers :: Maybe SortBy -> Handler [User]
sortUsers = liftIO . (<$> Db.listAllUsers) . sortBy
  where sortBy Nothing = id
        sortBy (Just Age) = sortWith age
        sortBy _ = sortWith name

addUser :: User -> Handler String
addUser = (>>= toHttpResponse) . liftIO . Db.addUser
  where toHttpResponse :: AddUserResult -> Handler String
        toHttpResponse (UserAdded msg) = return msg
        toHttpResponse (InvalidDate msg) = throwError err400 {errReasonPhrase = msg}
        toHttpResponse (UserAlreadyExisted msg) = throwError err400 {errReasonPhrase = msg}

deleteUser :: String -> Handler String
deleteUser user_name = liftIO (Db.deleteUser user_name) >>= toHttpResponse
  where toHttpResponse 0 = throwError err400 { errReasonPhrase = user_name ++ " not exists" }
        toHttpResponse _ = return $ user_name ++ " removed"
