{-# LANGUAGE OverloadedStrings #-}

module Link where

import System.Directory (doesFileExist)
import System.Posix.Files (createSymbolicLink, getSymbolicLinkStatus, isSymbolicLink)

symlinkFile :: FilePath -> FilePath -> IO ()
symlinkFile buildPath destPath = do
  isLink <- pathIsSymbolicLink destPath
  if not isLink
    then do
      putStrLn $ "→ :: " ++ destPath ++ "\n"
      createSymbolicLink buildPath destPath
    else putStrLn $ "Symlink " ++ destPath ++ " already exists - skipping.\n"

pathIsSymbolicLink :: FilePath -> IO Bool
pathIsSymbolicLink fp = do
  exists <- doesFileExist fp
  if exists
    then do
      status <- getSymbolicLinkStatus fp
      return (isSymbolicLink status)
    else return False
