{-# LANGUAGE FlexibleContexts #-}
module TypeSafeWS.Apis where

import GHC.Exts
import Servant
import Control.Monad.IO.Class
import Control.Exception.Base
import System.IO.Error
import Data.Int (Int64)
import GitHash
import Protolude hiding (msg)
import Prelude (String)
import qualified Data.ByteString.Char8 as BS8

import TypeSafeWS.ApiTypes
import TypeSafeWS.DataTypes

sortUsers :: Db -> Maybe SortBy -> Handler [User]
sortUsers Db{..} = liftIO . (<$> _listAllUsers) . sortBy
  where sortBy Nothing = identity
        sortBy (Just Age) = sortWith age
        sortBy _ = sortWith name

addUser :: Db -> User -> Handler String
addUser Db{..} = (>>= toHttpResponse) . liftIO . _addUser
  where toHttpResponse (Right msg) = return msg
        toHttpResponse (Left err) = throwError err400 {errReasonPhrase = msg err}

deleteUser :: Db -> String -> Handler String
deleteUser Db{..} = (>>= toHttpResponse) . liftIO . _deleteUser
  where toHttpResponse 0 = throwError err400 { errReasonPhrase = "user name not exists" }
        toHttpResponse _ = return "user removed"

getServiceInfo :: Handler ServiceInfo
getServiceInfo = liftIO (getGitInfo ".") >>= toHttpResponse
  where toHttpResponse (Right gi) =
          return $ ServiceInfo (giHash gi) (giBranch gi) (giCommitDate gi) (giCommitMessage gi)
        toHttpResponse (Left err) = throwError err500 {errReasonPhrase = show err}
