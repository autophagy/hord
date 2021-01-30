module Compile where

import Config (CompileMode (..))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text (pack)
import qualified Data.Text.IO
import qualified Dhall
import Dhall.JSON (dhallToJSON)
import Dhall.Yaml (defaultOptions, dhallToYaml)
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)

compileDhallToText :: FilePath -> FilePath -> IO ()
compileDhallToText srcPath buildPath =
  Dhall.input Dhall.auto (pack srcPath)
    >>= Data.Text.IO.writeFile buildPath

compileDhallToYaml :: FilePath -> FilePath -> IO ()
compileDhallToYaml srcPath buildPath =
  Data.Text.IO.readFile srcPath
    >>= dhallToYaml defaultOptions (Just srcPath)
    >>= B.writeFile buildPath

compileDhallToJSON :: FilePath -> FilePath -> IO ()
compileDhallToJSON srcPath buildPath = do
  f <- Data.Text.IO.readFile srcPath
  expr <- Dhall.inputExpr f
  case dhallToJSON expr of
    Left e -> print e
    Right jsonValue ->
      BL.writeFile buildPath (encodePretty jsonValue)

determineCompiler :: CompileMode -> (FilePath -> FilePath -> IO ())
determineCompiler Raw = copyFile
determineCompiler Text = compileDhallToText
determineCompiler YAML = compileDhallToYaml
determineCompiler JSON = compileDhallToJSON

compile :: FilePath -> FilePath -> CompileMode -> IO ()
compile srcPath buildPath compileMode = do
  createDirectoryIfMissing True $ takeDirectory buildPath
  putStrLn $ "λ [" ++ show compileMode ++ "] :: " ++ srcPath
  determineCompiler compileMode srcPath buildPath
