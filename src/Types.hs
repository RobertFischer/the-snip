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
  , optsVerbose :: Bool
  }

data App = App
  { appLogFunc :: LogFunc
  , appDoTrim :: Bool
  , appBaseDir :: Path Abs Dir
  , appFiles :: Set (Path Rel File)
  , appOutputDir :: Path Abs Dir
  }

instance HasLogFunc App where
  logFuncL :: Lens' App LogFunc
  logFuncL = lens appLogFunc
    (\it new -> it { appLogFunc = new })
