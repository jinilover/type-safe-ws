{-# LANGUAGE DeriveGeneric #-}

module TypeSafeWS.Git where

import Control.Exception
import System.Exit
import System.Process
import Data.Aeson.Types
import GHC.Generics
import Prelude hiding (FilePath)

import TypeSafeWS.DataTypes

newtype FilePath = FilePath String

getGitInfo :: FilePath -> IO GitInfo
getGitInfo root =
  let run = runGitCmd root in
  GitInfo <$> run ["rev-parse", "HEAD"]
          <*> run ["rev-parse", "--abbrev-ref", "HEAD"]
          <*> run ["log", "HEAD", "-1", "--format=%cd"]
          <*> run ["log", "-1", "--pretty=%B"]
  where runGitCmd (FilePath gitPath) args = do
          (ec, out, err) <- readCreateProcessWithExitCode (proc "git" args) { cwd = Just gitPath } ""
          case ec of
            ExitSuccess -> return $ takeWhile (/= '\n') out
            ExitFailure _ -> throwIO $ GitException gitPath args ec out err

data GitInfo = GitInfo {
  gitHash :: String
, gitBranch :: String
, gitCommitDate :: String
, gitCommitMessage :: String
} deriving Generic
instance ToJSON GitInfo

data GitException = GitException {
  gitPath :: String
, args :: [String]
, ec :: ExitCode
, out :: String
, err :: String
} deriving Show
instance Exception GitException
