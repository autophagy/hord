module Compile where

import Config (CompileMode (..))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Functor ((<&>))
import Data.Text (pack)
import qualified Data.Text.Encoding
import qualified Data.Text.IO
import qualified Dhall
import Dhall.JSON (dhallToJSON)
import Dhall.Yaml (defaultOptions, dhallToYaml)
import System.Directory (createDirectoryIfMissing)
import System.Exit (die)
import System.FilePath.Posix (takeDirectory)

compileDhallToText :: FilePath -> IO B.ByteString
compileDhallToText srcPath =
  Dhall.input Dhall.auto (pack srcPath)
    <&> Data.Text.Encoding.encodeUtf8

compileDhallToYaml :: FilePath -> IO B.ByteString
compileDhallToYaml srcPath =
  Data.Text.IO.readFile srcPath
    >>= dhallToYaml defaultOptions (Just srcPath)

compileDhallToJSON :: FilePath -> IO B.ByteString
compileDhallToJSON srcPath = do
  f <- Data.Text.IO.readFile srcPath
  expr <- Dhall.inputExpr f
  case dhallToJSON expr of
    Left e -> die $ "Internal Dhall -> JSON parsing error: " ++ show e
    Right jsonValue -> return $ BL.toStrict (encodePretty jsonValue)

determineCompiler :: CompileMode -> FilePath -> IO B.ByteString
determineCompiler Raw = B.readFile
determineCompiler Text = compileDhallToText
determineCompiler YAML = compileDhallToYaml
determineCompiler JSON = compileDhallToJSON

compile :: FilePath -> FilePath -> CompileMode -> IO ()
compile srcPath buildPath compileMode = do
  createDirectoryIfMissing True $ takeDirectory buildPath
  putStrLn $ "λ [" ++ show compileMode ++ "] :: " ++ srcPath
  compiled <- determineCompiler compileMode srcPath
  B.writeFile buildPath compiled
