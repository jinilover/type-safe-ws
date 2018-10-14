module TypeSafeWS.AppConfigSpec
  (specs) where

import Test.Hspec

import Protolude

import TypeSafeWS.Config
import TypeSafeWS.ConfigTypes

loadAppConfigSpec :: Spec
loadAppConfigSpec =
  describe "AppConfig spec" $
    it "load AppConfig successfully/correctly" $
       loadAppConfig >>= (`shouldBe`
         AppConfig {
            appPort = 9001,
            dbscriptsDir = "src/resources/dbscripts",
            dbConfig = DbConfig {
              dbHost = "localhost",
              dbName = "postgres",
              user = "postgres",
              password = "password",
              dbPort = 5445,
              noOfStripes = 2,
              idleTime = 60,
              stripeSize = 10
            }
          }
        )

specs :: [Spec]
specs = [loadAppConfigSpec]
