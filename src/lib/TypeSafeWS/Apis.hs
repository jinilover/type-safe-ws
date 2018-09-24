{-# LANGUAGE FlexibleContexts #-}
module TypeSafeWS.Apis where

import GHC.Exts
import Servant
import Control.Monad.IO.Class
import Control.Exception.Base
import System.IO.Error
import Data.Int (Int64)
import qualified Data.ByteString.Char8 as BS8

import TypeSafeWS.ApiTypes
import TypeSafeWS.DataTypes
import TypeSafeWS.Git
import qualified TypeSafeWS.DbServices as Db

sortUsers :: Maybe SortBy -> Handler [User]
sortUsers = liftIO . (<$> Db.listAllUsers) . sortBy
  where sortBy Nothing = id
        sortBy (Just Age) = sortWith age
        sortBy _ = sortWith name

addUser :: User -> Handler String
addUser = (>>= toHttpResponse) . liftIO . Db.addUser
  where toHttpResponse (Right msg) = return msg
        toHttpResponse (Left err) = throwError err400 {errReasonPhrase = msg err}

deleteUser :: String -> Handler String
deleteUser = (>>= toHttpResponse) . liftIO . Db.deleteUser
  where toHttpResponse 0 = throwError err400 { errReasonPhrase = "user name not exists" }
        toHttpResponse _ = return "user removed"

getServiceInfo :: Handler GitInfo
getServiceInfo = liftIO $ getGitInfo $ FilePath "."
