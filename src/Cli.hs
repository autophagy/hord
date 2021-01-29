module Cli where

import Compile (compile)
import Config (HordConf (..), Symlink (..), open)
import Control.Monad (unless)
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

hordify :: FilePath -> FilePath -> Bool -> Symlink -> IO ()
hordify workingDir buildDir compileOnly symlink = do
  let srcPath = workingDir ++ "/" ++ src symlink
  let buildPath = buildDir ++ "/_build/" ++ src symlink
  let destPath = dest symlink
  compile srcPath buildPath $ mode symlink
  unless compileOnly (symlinkFile buildPath destPath)

main :: IO ()
main = do
  parsedArgs <- parseArgs
  currentDir <- getCurrentDirectory
  let workingDir = currentDir ++ "/" ++ folder parsedArgs
  createDirectoryIfMissing True $ currentDir ++ "/_build/"
  hordConfig <- open workingDir
  mapM_ (hordify workingDir currentDir (compileOnly parsedArgs)) (hord hordConfig)
