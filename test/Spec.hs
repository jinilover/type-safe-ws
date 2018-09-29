import Test.Hspec

import qualified TypeSafeWS.AppConfigSpec as AppCfg
import qualified TypeSafeWS.DbServicesSpec as Db
import qualified TypeSafeWS.ApisSpec as Apis

main :: IO ()
main = hspec $ foldl (>>) (return ()) specs

specs :: [Spec]
specs = AppCfg.specs ++ Db.specs ++ Apis.specs
