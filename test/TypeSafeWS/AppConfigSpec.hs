module TypeSafeWS.AppConfigSpec where

import System.IO.Unsafe
import Test.Hspec

import TypeSafeWS.Config
import TypeSafeWS.ConfigTypes

loadAppConfigSpec :: Spec
loadAppConfigSpec =
  describe "AppConfig spec" $
    it "load AppConfig successfully/correctly" $
       unsafePerformIO loadAppConfig
        `shouldBe`
       AppConfig {
          appPort = 9001,
          dbscriptsDir = "src/resources/dbscripts",
          dbConfig = DbConfig {
            dbHost = "localhost",
            dbName = "postgres",
            user = "postgres",
            password = "password",
            dbPort = 5445
          }
        }
