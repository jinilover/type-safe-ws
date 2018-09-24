{-# LANGUAGE FlexibleContexts #-}
module TypeSafeWS.Apis where

import GHC.Exts
import Servant
import Control.Monad.IO.Class
import Control.Exception.Base
import System.IO.Error
import Data.Int (Int64)
import Database.PostgreSQL.Simple
import qualified Data.ByteString.Char8 as BS8

import TypeSafeWS.ApiTypes
import TypeSafeWS.DataTypes
import TypeSafeWS.Git
import qualified TypeSafeWS.DbServices as Db

sortUsers :: Connection -> Maybe SortBy -> Handler [User]
sortUsers conn = liftIO . (<$> Db.listAllUsers conn) . sortBy
  where sortBy Nothing = id
        sortBy (Just Age) = sortWith age
        sortBy _ = sortWith name

addUser :: Connection -> User -> Handler String
addUser conn = (>>= toHttpResponse) . liftIO . Db.addUser conn
  where toHttpResponse (Right msg) = return msg
        toHttpResponse (Left err) = throwError err400 {errReasonPhrase = msg err}

deleteUser :: Connection -> String -> Handler String
deleteUser conn = (>>= toHttpResponse) . liftIO . Db.deleteUser conn
  where toHttpResponse 0 = throwError err400 { errReasonPhrase = "user name not exists" }
        toHttpResponse _ = return "user removed"

getServiceInfo :: Handler GitInfo
getServiceInfo = liftIO $ getGitInfo $ FilePath "."
