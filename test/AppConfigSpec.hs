module AppConfigSpec where

import System.IO.Unsafe
import Test.Hspec
import Config
import ConfigTypes

loadAppConfigSpec :: Spec
loadAppConfigSpec =
  describe "AppConfig spec" $
    it "load AppConfig successfully/correctly" $
       unsafePerformIO (loadAppConfig "src/resources/appl.cfg")
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
