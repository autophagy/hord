{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Text (pack)
import Dhall

data Symlink = Symlink {src :: FilePath, dest :: FilePath} deriving (Generic, FromDhall)

newtype HordConf = HordConf {hord :: [Symlink]} deriving (Generic, FromDhall)

open :: FilePath -> IO HordConf
open f = input Dhall.auto (pack $ f ++ "/hord.dhall")
