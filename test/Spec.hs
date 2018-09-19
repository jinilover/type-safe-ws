import Test.Hspec

import TypeSafeWS.AppConfigSpec

main :: IO ()
main = hspec $ foldl (>>) (return ()) specs

specs :: [Spec]
specs = [loadAppConfigSpec]
