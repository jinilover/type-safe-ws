module TypeSafeWS.AppConfigSpec
  (specs) where

import Test.Hspec

import Protolude
import System.Environment

import TypeSafeWS.Config
import TypeSafeWS.ConfigTypes

loadAppConfigSpec :: Spec
loadAppConfigSpec =
  describe "AppConfig spec" $
    before setEnvs $
    it "load AppConfig successfully/correctly" $
       loadAppConfig [] >>= (`shouldBe`
         AppConfig {
            appPort = 9001,
            dbConfig = DbConfig {
              dbHost = dbHost',
              dbName = dbName',
              user = dbUser',
              password = dbPassword',
              dbPort = dbPort',
              noOfStripes = 2,
              idleTime = 60,
              stripeSize = 10
            }
          }
        )

setEnvs :: IO ()
setEnvs = do
  setEnv "DB_HOST" dbHost'
  setEnv "DB_NAME" dbName'
  setEnv "DB_USER" dbUser'
  setEnv "DB_PASSWORD" dbPassword'
  setEnv "DB_PORT" dbPort'

dbHost' = "localhost"
dbName' = "postgres"
dbUser' = "postgres"
dbPassword' = "password"
dbPort' = "5445"

specs :: [Spec]
specs = [loadAppConfigSpec]
