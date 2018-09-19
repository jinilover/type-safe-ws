{-# LANGUAGE GADTs #-}

module TypeSafeWS.Services where

import GHC.Exts

import TypeSafeWS.ApiTypes
import TypeSafeWS.DataTypes

someFunc :: IO ()
someFunc = putStrLn "someFunc!!"

listAll :: [User]
listAll = users

sortBy :: Maybe SortBy -> [User]
sortBy Nothing = users
sortBy (Just Age) = sortWith age users
sortBy _ = sortWith name users
