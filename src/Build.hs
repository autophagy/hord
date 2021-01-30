module Build where

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
import System.Exit (die)

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

compile :: CompileMode -> FilePath -> IO B.ByteString
compile Raw = B.readFile
compile Text = compileDhallToText
compile YAML = compileDhallToYaml
compile JSON = compileDhallToJSON

build :: FilePath -> FilePath -> CompileMode -> IO ()
build srcPath buildPath compileMode = do
  putStrLn $ "λ [" ++ show compileMode ++ "] :: " ++ srcPath
  compiled <- compile compileMode srcPath
  B.writeFile buildPath compiled
