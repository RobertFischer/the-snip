{-# LANGUAGE NoImplicitPrelude #-}
module Types
  ( App (..)
  , Options (..)
  ) where

import RIO
import Path

-- | Command line arguments
data Options = Options
  { optsRecursive :: Bool
  , optsPaths :: [FilePath]
  , optsBaseDir :: Maybe (SomeBase Dir)
  , optsOutputDir :: Maybe (SomeBase Dir)
  , optsTrim :: Bool
  }

data App = App
  { appDoTrim :: Bool
  , appBaseDir :: Path Abs Dir
  , appFiles :: Set (Path Rel File)
  , appOutputDir :: Path Abs Dir
  }


