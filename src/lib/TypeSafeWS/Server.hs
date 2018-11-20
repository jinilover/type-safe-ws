module TypeSafeWS.Server where

import Servant
import Network.Wai.Handler.Warp
import Prelude (return)

import TypeSafeWS.DataTypes
import TypeSafeWS.ApiTypes
import TypeSafeWS.Apis

server :: Db -> Server RestAPI
server db = return "Welcome to microservice in pure FP"
              :<|> getServiceInfo
              :<|> sortUsers db
              :<|> addUser db
              :<|> deleteUser db

serviceApi :: Proxy RestAPI
serviceApi = Proxy
