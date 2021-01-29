{-# LANGUAGE OverloadedStrings #-}

module Compile where

import Config (CompileMode (..))
import Data.Text (pack)
import qualified Data.Text.IO
import qualified Dhall
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)

createBuildDirectory :: FilePath -> IO ()
createBuildDirectory workingDir =
  createDirectoryIfMissing True (workingDir ++ "/_build")

compileDhallToText :: FilePath -> FilePath -> IO ()
compileDhallToText srcPath buildPath = do
  putStrLn $ "λ [Text] :: " ++ srcPath
  compiledText <- Dhall.input Dhall.auto $ pack srcPath
  Data.Text.IO.writeFile buildPath compiledText

determineCompiler :: CompileMode -> (FilePath -> FilePath -> IO ())
determineCompiler Raw = copyFile
determineCompiler Dhall = compileDhallToText

compile :: FilePath -> FilePath -> CompileMode -> IO ()
compile srcPath buildPath compileMode = do
  createDirectoryIfMissing True $ takeDirectory buildPath
  determineCompiler compileMode srcPath buildPath
