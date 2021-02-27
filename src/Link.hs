module Link where

import Data.Functor ((<&>))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix (takeDirectory)
import System.Posix.Files (createSymbolicLink, getSymbolicLinkStatus, isSymbolicLink, removeLink)

symlinkFile :: FilePath -> FilePath -> IO ()
symlinkFile buildPath destPath = do
  isLink <- pathIsSymbolicLink destPath
  if not isLink
    then do
      putStrLn $ "→ :: " ++ destPath ++ "\n"
      createDirectoryIfMissing True $ takeDirectory destPath
      createSymbolicLink buildPath destPath
    else putStrLn $ "Symlink " ++ destPath ++ " already exists - skipping.\n"

pathIsSymbolicLink :: FilePath -> IO Bool
pathIsSymbolicLink fp = do
  exists <- doesFileExist fp
  if exists
    then
      getSymbolicLinkStatus fp
        <&> isSymbolicLink
    else pure False

cleanSymlink :: FilePath -> IO ()
cleanSymlink destPath = do
  putStrLn $ "Cleaning " ++ destPath
  removeLink destPath
