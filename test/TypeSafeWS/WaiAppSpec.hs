{-# LANGUAGE QuasiQuotes #-}
module TypeSafeWS.WaiAppSpec
  (specs) where

import Test.Hspec
import Test.Hspec.Wai -- hiding (post)
import Test.Hspec.Wai.JSON

import qualified Network.HTTP.Types.Header as HTTP
import qualified Data.ByteString.Char8 as BS8
import Servant
import Network.HTTP.Types.Method
import Network.Wai.Handler.Warp
import Database.PostgreSQL.Simple.Time
import Control.Arrow
import Data.Bifunctor
import Data.Aeson (Value(..), object, (.=))
import Protolude hiding (get)

import TypeSafeWS.Server
import TypeSafeWS.DataTypes

import TypeSafeWS.MockData

-- Spec for testing the http responses

createApp :: Db -> IO Application
createApp = return . serve serviceApi . server

successRespSpec :: Spec
successRespSpec = with (createApp db) $
  describe "happy day scenarios" $ do
    it "get / responds with welcome message" $
      get "/" `shouldRespondWith` "Welcome to microservice in pure FP"
    it "get /users responds with users in json" $
      get "/users" `shouldRespondWith` users {matchStatus = 200, matchHeaders = jsonContent}
    it "get /users?sortBy=name responds with users sorted by name" $
      get "/users?sortBy=name" `shouldRespondWith` usersByName {matchStatus = 200, matchHeaders = jsonContent}
    it "get /users?sortBy=age responds with users sorted by age" $
      get "/users?sortBy=age" `shouldRespondWith` usersByAge {matchStatus = 200, matchHeaders = jsonContent}
    it "delete /users/user_name responds with success msg" $
      delete "/users/Isaac%20Newton" `shouldRespondWith` "user removed"
    it "post /users sample user responds with success msg" $
      -- post "/users" postBody `shouldRespondWith` "user added"
      request methodPost "/users" postHeader postBody `shouldRespondWith` "user added"
  where jsonContent = ["Content-Type" <:> "application/json;charset=utf-8"]
        users = [json|[
         {"email":"isaac@newton.co.uk", "registrationDate":"1683-03-01", "age":372, "name":"Isaac Newton"}
        ,{"email":"ae@mc2.org", "registrationDate":"1905-12-01", "age":136, "name":"Albert Einstein"}
        ,{"email":"kg@uv.edu", "registrationDate":"1906-04-28", "age":112, "name":"Kurt Goedel"}
        ]|]
        usersByName = [json|[
         {"email":"ae@mc2.org", "registrationDate":"1905-12-01", "age":136, "name":"Albert Einstein"}
        ,{"email":"isaac@newton.co.uk", "registrationDate":"1683-03-01", "age":372, "name":"Isaac Newton"}
        ,{"email":"kg@uv.edu", "registrationDate":"1906-04-28", "age":112, "name":"Kurt Goedel"}
        ]|]
        usersByAge = [json|[
         {"email":"kg@uv.edu", "registrationDate":"1906-04-28", "age":112, "name":"Kurt Goedel"}
        ,{"email":"ae@mc2.org", "registrationDate":"1905-12-01", "age":136, "name":"Albert Einstein"}
        ,{"email":"isaac@newton.co.uk", "registrationDate":"1683-03-01", "age":372, "name":"Isaac Newton"}
        ]|]
        postBody = [json|
        {"email":"isaac@newton.co.uk", "registrationDate":"1683-03-01", "age":372, "name":"Isaac Newton"}
        |]

invalidDateRespSpec :: Spec
invalidDateRespSpec = with (createApp db) $
  describe "add user failed due to invalid date" $
    it "post /users invalid date responds with 400" $
      request methodPost "/users" postHeader postBody `shouldRespondWith` 400
      where postBody = [json|
        {"email":"isaac@newton.co.uk", "registrationDate":"1683-03-1xx", "age":372, "name":"Isaac Newton"}
        |]

duplicatedUserRespSpec :: Spec
duplicatedUserRespSpec = with (createApp db {_addUser = const . return . Left . UserAlreadyExisted $ "user already existed"}) $
  describe "add user failed due to user already existed" $
    it "post /users duplicated user responds with 400" $
      request methodPost "/users" postHeader postBody `shouldRespondWith` 400
      where postBody = [json|
        {"email":"isaac@newton.co.uk", "registrationDate":"1683-03-01", "age":372, "name":"Isaac Newton"}
        |]

nonexistUserRespSpec :: Spec
nonexistUserRespSpec = with (createApp db {_deleteUser = const $ return 0}) $ -- 0 record deleted
  describe "delete non-exist user" $
    it "delete /users/non-exist responds with 400" $
      delete "/users/Isaac%20Newton" `shouldRespondWith` 400

postHeader :: [HTTP.Header]
postHeader = [("Content-Type", "application/json")]

db :: Db
db = Db {
        _listAllUsers = return [isaac, albert, kurt]
      , _addUser = return . bimap InvalidDate (const "user added") . parseDay . BS8.pack . registrationDate
      , _deleteUser = const $ return 1 -- 1 record deleted
      }

specs :: [Spec]
specs = [successRespSpec, invalidDateRespSpec, duplicatedUserRespSpec, nonexistUserRespSpec]
