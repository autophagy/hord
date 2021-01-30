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
compileDhallToText srcPath buildPath = do
  putStrLn $ "λ [Text] :: " ++ srcPath
  compiledText <- Dhall.input Dhall.auto $ pack srcPath
  Data.Text.IO.writeFile buildPath compiledText

compileDhallToYaml :: FilePath -> FilePath -> IO ()
compileDhallToYaml srcPath buildPath = do
  putStrLn $ "λ [YAML] :: " ++ srcPath
  f <- Data.Text.IO.readFile srcPath
  compiledYaml <- dhallToYaml defaultOptions (Just srcPath) f
  B.writeFile buildPath compiledYaml

compileDhallToJSON :: FilePath -> FilePath -> IO ()
compileDhallToJSON srcPath buildPath = do
  putStrLn $ "λ [JSON] :: " ++ srcPath
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
  determineCompiler compileMode srcPath buildPath
