module TypeSafeWS.Utils
  where

import Prelude (String)

resourceFolder :: [String] -> String
resourceFolder [] = "src/resources/"
resourceFolder (folder : _) = folder
