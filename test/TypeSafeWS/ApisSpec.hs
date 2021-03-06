module TypeSafeWS.ApisSpec
  (specs) where

import Test.Hspec

import Protolude
import Servant

import TypeSafeWS.DataTypes
import TypeSafeWS.Apis
import TypeSafeWS.ApiTypes

import TypeSafeWS.MockData

sortUsersSpec :: Spec
sortUsersSpec =
  describe "sortUsersSpec" $ do
    it "list user w/o sorting" $
      io (usersFromMock Nothing) >>= (`shouldBe` Right [isaac, kurt, albert])
    it "sort user by age" $
      io (usersFromMock $ Just Age) >>= (`shouldBe` Right [kurt, albert, isaac])
    it "sort user by name" $
      io (usersFromMock $ Just Name) >>= (`shouldBe` Right [albert, isaac, kurt])
  where usersFromMock = sortUsers db {_listAllUsers = return [isaac, kurt, albert]}

addUserSpec :: Spec
addUserSpec =
  describe "addUserSpec" $ do
    it "handled successful adding user" $
      let result = addUserToMock db {_addUser = return . Right . name} in
      io result >>= (`shouldBe` Right (name albert))
    it "handled adding duplicated user" $
      let result = addUserToMock db {_addUser = const . return . Left $ UserAlreadyExisted err} in
      io result >>= (`shouldBe` Left (err400 {errReasonPhrase = err}))
    it "handled invalid registrationDate" $
      let result = addUserToMock db {_addUser = const . return . Left $ InvalidDate err} in
      io result >>= (`shouldBe` Left (err400 {errReasonPhrase = err}))
  where err = "sample error"
        addUserToMock = (`addUser` albert)

deleteUserSpec :: Spec
deleteUserSpec =
  describe "deleteUserSpec" $ do
    it "delete existing user" $
      let result = removeUserFromMock db {_deleteUser = const $ return 1} in
      io result >>= (`shouldBe` Right "user removed")
    it "delete non-exist user" $
      let result = removeUserFromMock db {_deleteUser = const $ return 0} in
      io result >>= (`shouldBe` Left (err400 {errReasonPhrase = "user name not exists"}))
  where removeUserFromMock = (`deleteUser` name albert)

db = Db undefined undefined undefined

io :: Handler a -> IO (Either ServantErr a)
io = runExceptT . runHandler'

specs :: [Spec]
specs = [sortUsersSpec, addUserSpec, deleteUserSpec]
