{-# LANGUAGE GADTs #-}

module Services where

import ApiTypes
import DataTypes
import GHC.Exts

someFunc :: IO ()
someFunc = putStrLn "someFunc!!"

listAll :: [User]
listAll = users

sortBy :: Maybe SortBy -> [User]
sortBy Nothing = users
sortBy (Just Age) = sortWith age users
sortBy _ = sortWith name users
