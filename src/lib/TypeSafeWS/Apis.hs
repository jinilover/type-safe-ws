module TypeSafeWS.Apis where

import GHC.Exts
import Servant
import Control.Monad.IO.Class
import Data.Int (Int64)

import TypeSafeWS.ApiTypes
import TypeSafeWS.DataTypes
import qualified TypeSafeWS.DbServices as Db
import qualified Data.ByteString.Char8 as BS8

sortUsers :: Maybe SortBy -> Handler [User]
sortUsers sortParam = liftIO $ sortBy sortParam <$> Db.listAllUsers
  where sortBy Nothing = id
        sortBy (Just Age) = sortWith age
        sortBy _ = sortWith name

addUser :: User -> Handler String
addUser = liftIO . Db.addUser

deleteUser :: String -> Handler String
deleteUser user_name = liftIO (Db.deleteUser user_name) >>= handleResult
  where handleResult :: Int64 -> Handler String
        handleResult 0 = throwError err400 { errReasonPhrase = user_name ++ " already removed" }
        handleResult _ = return $ user_name ++ " removed"
