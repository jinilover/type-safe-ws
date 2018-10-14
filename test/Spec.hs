import Test.Hspec

import Protolude

import qualified TypeSafeWS.AppConfigSpec as AppCfg
import qualified TypeSafeWS.DbServicesSpec as Db
import qualified TypeSafeWS.ApisSpec as Apis
import qualified TypeSafeWS.WaiAppSpec as WaiApp

main :: IO ()
main = hspec $ foldl (>>) (return ()) specs

specs :: [Spec]
specs = AppCfg.specs ++ Db.specs ++ Apis.specs ++ WaiApp.specs
