{-# LANGUAGE GADTs #-}

module TypeSafeWS.Apis where

import GHC.Exts

import TypeSafeWS.ApiTypes
import TypeSafeWS.DataTypes

sortBy :: Maybe SortBy -> [User] -> [User]
sortBy Nothing = id
sortBy (Just Age) = sortWith age
sortBy _ = sortWith name
