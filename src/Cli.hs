module Cli where

import Build (build, determineMode)
import Config (HordConf (..), Symlink (Symlink), open)
import Control.Monad (unless)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.SHA (sha1, showDigest)
import Link (symlinkFile)
import Options.Applicative
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)

data Args = Args {folder :: FilePath, compileOnly :: Bool} deriving (Show)

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
hordify workingDir buildDir compileOnly (Symlink src dest) = do
  let srcPath = workingDir ++ "/" ++ src
  let buildPath = buildDir ++ hashFilePath dest
  build srcPath buildPath $ determineMode src dest
  unless compileOnly $ symlinkFile buildPath dest

main :: IO ()
main = do
  parsedArgs <- parseArgs
  currentDir <- getCurrentDirectory
  let workingDir = currentDir ++ "/" ++ folder parsedArgs
  let buildDir = currentDir ++ "/_build/"
  createDirectoryIfMissing True buildDir
  hordConfig <- open workingDir
  mapM_ (hordify workingDir buildDir (compileOnly parsedArgs)) (hord hordConfig)
