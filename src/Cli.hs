module Cli where

import Build (build, determineMode)
import Config (HordConf (..), Symlink (Symlink), open)
import Control.Monad (unless)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.SHA (sha1, showDigest)
import qualified Link as L (cleanSymlink, symlinkFile)
import Options.Applicative
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath (takeFileName)

data Args = Args {folder :: FilePath, compileOnly :: Bool, clean :: Bool} deriving (Show)

args :: Parser Args
args =
  Args
    <$> argument
      str
      ( metavar "TARGET"
          <> help "Target folder to deploy Hord (Must contain a hord.dhall file"
      )
    <*> switch
      ( long "compileOnly"
          <> short 'c'
          <> help "Compile to _build/ only - no symlinking."
      )
    <*> switch
      ( long "clean"
          <> help "Remove all defined dest files before compilation/symlinking."
      )

parseArgs :: IO Args
parseArgs = execParser opts
  where
    opts =
      info
        (args <**> helper)
        ( fullDesc
            <> progDesc "Hord :: A dotfile compilation/deployment tool"
        )

hashFilePath :: FilePath -> String
hashFilePath = showDigest . sha1 . fromString

hordify :: FilePath -> FilePath -> Bool -> Symlink -> IO ()
hordify workingDir buildDir compileOnlyFlag (Symlink src dest) = do
  let srcPath = workingDir ++ "/" ++ src
  let buildPath = concat [buildDir, hashFilePath dest, "_", takeFileName dest]
  build srcPath buildPath $ determineMode src dest
  unless compileOnlyFlag $ L.symlinkFile buildPath dest

cleanSymlink :: Symlink -> IO ()
cleanSymlink (Symlink _ dest) = L.cleanSymlink dest

main :: IO ()
main = do
  parsedArgs <- parseArgs
  currentDir <- getCurrentDirectory
  let workingDir = currentDir ++ "/" ++ folder parsedArgs
  let buildDir = currentDir ++ "/_build/"
  createDirectoryIfMissing True buildDir
  hordConfig <- open workingDir
  if clean parsedArgs
    then mapM_ cleanSymlink (hord hordConfig)
    else pure ()
  mapM_ (hordify workingDir buildDir (compileOnly parsedArgs)) (hord hordConfig)
