import Test.Hspec

import AppConfigSpec

main :: IO ()
main = hspec $ foldl (>>) (return ()) specs

specs :: [Spec]
specs = [loadAppConfigSpec]
