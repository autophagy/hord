module Build where

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import Data.Text (pack)
import qualified Data.Text.Encoding
import qualified Data.Text.IO
import qualified Dhall
import Dhall.JSON (dhallToJSON)
import Dhall.Yaml (defaultOptions, dhallToYaml)
import System.Exit (die)
import Say

data BuildMode = Raw | Text | YAML | JSON deriving (Show, Eq)

determineMode :: FilePath -> FilePath -> BuildMode
determineMode src dest
  | not $ ".dhall" `isSuffixOf` src = Raw
  | any (`isSuffixOf` dest) [".yml", ".yaml"] = YAML
  | ".json" `isSuffixOf` dest = JSON
  | otherwise = Text

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
    Right jsonValue -> pure $ BL.toStrict (encodePretty jsonValue)

compile :: BuildMode -> FilePath -> IO B.ByteString
compile Raw = B.readFile
compile Text = compileDhallToText
compile YAML = compileDhallToYaml
compile JSON = compileDhallToJSON

build :: FilePath -> FilePath -> BuildMode -> IO ()
build srcPath buildPath buildMode = do
  sayString $ "λ [" ++ show buildMode ++ "] :: " ++ srcPath
  compiled <- compile buildMode srcPath
  B.writeFile buildPath compiled
