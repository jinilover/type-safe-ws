{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module TypeSafeWS.DbServicesSpec where

import Database.PostgreSQL.Simple
import Control.Monad
import Data.Int (Int64)
import Data.Pool
import Data.Set
import Data.List
import Test.Hspec

import TypeSafeWS.DbServices
import TypeSafeWS.Config
import TypeSafeWS.ConfigTypes
import TypeSafeWS.DataTypes

usersSpec :: Spec
usersSpec = beforeAll setup $
  describe "usersSpec" $ do
    it "added users successfully" $
      let users = [User "Isaac Newton" 372 "isaac@newton.co.uk" "1683-03-01"
                 , User "Albert Einstein" 136 "ae@mc2.org" "1905-12-01"]
          result = getConnPool >>= (`withResource` (`traverse` users) . addUser)
          expected = Right <$> ["User Isaac Newton created", "User Albert Einstein created"] in
      result >>= (`shouldBe` expected)
    it "retrieve the same users" $
      let expected = fromDistinctAscList [User "Isaac Newton" 372 "isaac@newton.co.uk" "1683-03-01"
                     , User "Albert Einstein" 136 "ae@mc2.org" "1905-12-01"] in
      fromDistinctAscList <$> retrieveUsers >>= (`shouldBe` expected)
    it "invalid registrationDate" $
      let user = User "Isaac Newton" 0 "" "1683-03-1"
          result = getConnPool >>= (`withResource` (`addUser` user)) in
      result >>= (`shouldSatisfy` (\x -> case x of
        (Left (InvalidDate msg)) -> "not enough input from date 1683-03-1" `isInfixOf` msg
        _ -> False)
      )
    it "duplicated primary key not allowed" $
      let user = User "Isaac Newton" 0 "" "1683-03-01"
          result = getConnPool >>= (`withResource` (`addUser` user)) in
      result >>= (`shouldSatisfy` (\x -> case x of
        (Left (UserAlreadyExisted msg)) -> "Key (user_name)=(Isaac Newton) already exists" `isInfixOf` msg
        _ -> False)
      )
    it "delete existing user" $
      deleteIssac >>= (`shouldBe` 1)
    it "retrieve remaining users" $
      retrieveUsers >>= (`shouldBe` [User "Albert Einstein" 136 "ae@mc2.org" "1905-12-01"])
    it "delete non-exist user" $
      deleteIssac >>= (`shouldBe` 0)

  where deleteIssac = getConnPool >>= (`withResource` (`deleteUser` "Isaac Newton"))
        retrieveUsers = getConnPool >>= (`withResource` listAllUsers)

setup :: IO ()
setup = do
  AppConfig{..} <- loadAppConfig
  getConnPool >>= (`withResource` (\c -> migrateDb c dbscriptsDir >> cleanupTables c))

getConnPool :: IO (Pool Connection)
getConnPool = loadAppConfig >>= initConnPool . dbConfig

cleanupTables :: Connection -> IO ()
cleanupTables conn = void $ execute_ conn "DELETE FROM users"

specs :: [Spec]
specs = [usersSpec]
