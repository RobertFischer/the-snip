{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Run
import Options.Applicative.Simple
import qualified Paths_the_snip
import RIO.Set qualified as S
import System.Posix.Files (getFileStatus, isDirectory, isRegularFile)
import Path.Posix
import Path.IO

description :: String
description = 
  "Within text files, use " <> show snipStartT <> 
  " on a line to denote that a snippet should start" <>
  " and " <> show snipEndT <> " to show that the snippet " <>
  " should end."

optionsParser :: Parser Options
optionsParser = Options
  <$> recursiveParser
  <*> many pathParser
  <*> optional baseDirParser
  <*> optional outputDirParser
  <*> trimParser
  <*> verboseParser
  where
    pathParser :: Parser FilePath
    pathParser = strArgument 
      ( metavar "PATH" 
      <> help "Path to input files and directories. May be specified multiple times."
      )
    trimParser = switch
      ( short 't'
      <> long "trim"
      <> help "Trim common whitespace"
      )
    verboseParser = switch
      ( short 'v'
      <> long "verbose"
      <> help "Be verbose in logging"
      )
    recursiveParser = switch
      ( short 'r'
      <> long "recurse"
      <> help "Recurse into subdirectories. If no path is given, recurse within the base directory."
      )
    baseDirParser = option someDirReadM
      ( short 'b'
      <> long "basedir"
      <> metavar "DIR"
      <> help "Base input directory. Defaults to the current working directory."
      )
    outputDirParser = option someDirReadM
      ( short 'o'
      <> long "outdir"
      <> metavar "DIR"
      <> help "Root directory for output. Will be created if it does not already exist. Defaults to the current working directory."
      )
    someDirReadM = maybeReader parseSomeDir

main :: IO ()
main = do
  (Options{..}, ()) <- simpleOptions
    $(simpleVersion Paths_the_snip.version)
    "Extract demarkated snippets from text files"
    description
    optionsParser
    empty
  baseDir <- optToAbsDir optsBaseDir
  logOpts <- logOptionsHandle stderr optsVerbose
  withLogFunc logOpts $ \logFunc -> do
    app <- App logFunc optsTrim baseDir
      <$> mkFiles baseDir optsRecursive optsPaths
      <*> optToAbsDir optsOutputDir
    runRIO app run

optToAbsDir :: Maybe (SomeBase Dir) -> IO (Path Abs Dir)
optToAbsDir = \case
  Nothing -> getCurrentDir
  Just (Abs absDir) -> return absDir
  Just (Rel relDir) -> (</> relDir) <$> getCurrentDir

mkFiles :: Path Abs Dir -> Bool -> [FilePath] -> IO (Set (Path Rel File))
mkFiles baseDir False [] = S.fromList . snd <$> listDirRel baseDir
mkFiles baseDir True [] = S.fromList . snd <$> listDirRecurRel baseDir
mkFiles baseDir recur (filepath:rest) = do
  restAsync <- async $
    if null rest then
      return S.empty
    else
      mkFiles baseDir recur rest
  fstat <- getFileStatus filepath
  if isDirectory fstat then do
    dir <- parseSomeDir filepath <&> 
      (\case
        (Abs absDir) -> absDir
        (Rel relDir) -> baseDir </> relDir
      )
    absFiles <- snd <$>
      if recur then
        listDirRecur dir
      else
        listDir dir
    relFiles <- sequence $ stripProperPrefix baseDir <$> absFiles
    S.union (S.fromList relFiles) <$> wait restAsync
  else if isRegularFile fstat then do
    file <- parseSomeFile filepath >>= \case
      Abs absFile -> stripProperPrefix baseDir absFile
      Rel relFile -> return relFile
    S.insert file <$> wait restAsync
  else
    fail $ "Found neither dir nor file at provided path " <> show filepath

