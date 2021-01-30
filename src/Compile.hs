module Compile where

import Config (CompileMode (..))
import qualified Data.ByteString
import Data.Text (pack)
import qualified Data.Text.IO
import qualified Dhall
import Dhall.Yaml (defaultOptions, dhallToYaml)
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)

compileDhallToText :: FilePath -> FilePath -> IO ()
compileDhallToText srcPath buildPath = do
  putStrLn $ "λ [Text] :: " ++ srcPath
  compiledText <- Dhall.input Dhall.auto $ pack srcPath
  Data.Text.IO.writeFile buildPath compiledText

compileDhallToYaml :: FilePath -> FilePath -> IO ()
compileDhallToYaml srcPath buildPath = do
  putStrLn $ "λ [YAML] :: " ++ srcPath
  f <- Data.Text.IO.readFile srcPath
  compiledYaml <- dhallToYaml defaultOptions (Just srcPath) f
  Data.ByteString.writeFile buildPath compiledYaml

determineCompiler :: CompileMode -> (FilePath -> FilePath -> IO ())
determineCompiler Raw = copyFile
determineCompiler Text = compileDhallToText
determineCompiler YAML = compileDhallToYaml

compile :: FilePath -> FilePath -> CompileMode -> IO ()
compile srcPath buildPath compileMode = do
  createDirectoryIfMissing True $ takeDirectory buildPath
  determineCompiler compileMode srcPath buildPath
