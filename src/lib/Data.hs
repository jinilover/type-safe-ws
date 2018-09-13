module Data where

import ApiType
import Data.Time.Calendar

users :: [User]
users = [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
        , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
        ]
