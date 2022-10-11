module Run (run, snipStartT, snipEndT) where

import Import
import Path.Posix
import Path.IO
import Data.Attoparsec.Text as Parser
import RIO.Text qualified as T
import RIO.List qualified as L

data LeadingWS = NoLeadingWS | LeadingSpaces Word | LeadingTabs Word deriving (Show)

data ParsedLine = EmptyLine | StartMarkerLine (Maybe (Path Rel File)) | EndMarkerLine (Maybe (Path Rel File)) | ContentLine (LeadingWS,Text) deriving (Show)

mkContentLine :: Text -> ParsedLine
mkContentLine content = ContentLine $ case T.uncons content of
  Nothing -> (NoLeadingWS, "")
  Just ('\t', _) -> 
    let (tabs,rest) = T.span ('\t' ==) content in
    (LeadingTabs (fromIntegral $ T.length tabs), T.copy rest)
  Just (' ', _) -> 
    let (spaces,rest) = T.span (' ' ==) content in
    (LeadingSpaces (fromIntegral $ T.length spaces), T.copy rest)
  Just _ -> 
    (NoLeadingWS, content)

parseLine :: Text -> RIO App ParsedLine
parseLine content = 
    case result of 
      Left err -> do
        logInfo . display . T.pack $
          "Defaulting back to content line: " <> err
        return $ mkContentLine content
      Right val -> do
        logInfo . display . T.pack $
          "Parsed non-content line: " <> show val
        return val
  where
    result = (`parseOnly` content) $ 
      parseEmptyLine <|> parseEndMarker <|> parseStartMarker

snipStartT :: Text
snipStartT = "<<<@snip"

parseEmptyLine :: Parser ParsedLine
parseEmptyLine =
  skipSpace >> endOfInput >> return EmptyLine

parseStartMarker :: Parser ParsedLine
parseStartMarker = 
    parseStartMarkerBeginning >> 
      (StartMarkerLine <$> (parseLabel <|> return Nothing))
  where
    parseStartMarkerBeginning = 
      string snipStartT <|>
        (Parser.take 1 >> parseStartMarkerBeginning)

snipEndT :: Text
snipEndT = "@snip>>>"

parseEndMarker :: Parser ParsedLine
parseEndMarker = 
    parseEndMarkerBeginning >>
      (EndMarkerLine <$> (parseLabel <|> return Nothing))
  where
    parseEndMarkerBeginning = 
      string snipEndT <|>
        (Parser.take 1 >> parseEndMarkerBeginning)

parseLabel :: Parser (Maybe (Path Rel File))
parseLabel = do
  skipSpace
  lbl <- Parser.takeTill Parser.isHorizontalSpace
  return $ parseRelFile (T.unpack lbl)

run :: RIO App ()
run = ask >>= \App{appFiles} ->
  pooledMapConcurrently_ runFile appFiles

runFile :: Path Rel File -> RIO App ()
runFile relFile = ask >>= \App{appBaseDir,appOutputDir} -> do
    let inFile = appBaseDir </> relFile
    let outDirBase = appOutputDir </> relFile
    outDir <- addExtension ".snips" outDirBase >>= addExtension ".d" >>= fileToDir
    logInfo . display . T.pack $ 
      "Processing file " <> show inFile <> " into dir " <> show outDir
    processFile inFile outDir
  where
    fileToDir = parseAbsDir . toFilePath

processFile :: Path Abs File -> Path Abs Dir -> RIO App ()
processFile inFile outDir = do
  fileLines <- liftIO $ T.lines <$> readFileUtf8 (toFilePath inFile)
  fileContents <- pooledMapConcurrently parseLine fileLines
  fileEx <- (".snip" <>) <$> fileExtension inFile
  logInfo . display . T.pack $ 
    "File contents for " <> show inFile <> ": " <> show fileContents
  processContents fileEx outDir 0 fileContents

processContents :: String -> Path Abs Dir -> Word -> [ParsedLine] -> RIO App ()
processContents _ _ _ [] = return ()
processContents fileExt outDir startCount (StartMarkerLine mayLbl:rest) = do
    logInfo . display . T.pack $
      "Saw the start of snippet for " <> show outDir <> ": " <> lbl
    outFile <- (outDir </>) <$> parseRelFile (lbl <> fileExt)
    concurrently_
      (processSnip outFile lbl rest)
      (processContents fileExt outDir (startCount+1) rest)
  where
    lbl = maybe (show startCount) toFilePath mayLbl
processContents fileExt outDir startCount (_:rest) = processContents fileExt outDir startCount rest

processSnip :: Path Abs File -> String -> [ParsedLine] -> RIO App ()
processSnip outFile _ [] =
  logInfo . display . T.pack $
    "No contents for snippet for " <> show outFile
processSnip outFile lbl (EmptyLine:remainingLines) =
  processSnip outFile lbl remainingLines
processSnip outFile lbl remainingLines = do
  contentT <- T.dropWhile ('\n' ==) . T.dropWhileEnd ('\n' ==) . T.unlines <$> extractSnipContent lbl remainingLines
  ensureDir $ parent outFile
  logInfo . display . T.pack $
    "Snippet for " <> show outFile <> ": " <> T.unpack contentT
  writeFileUtf8 (toFilePath outFile) contentT

extractSnipContent :: String -> [ParsedLine] -> RIO App [Text]
extractSnipContent _ [] = return []
extractSnipContent _ [EmptyLine] = return []
extractSnipContent lbl (EmptyLine:rest) = 
  ("" :) <$> extractSnipContent lbl rest
extractSnipContent _ (EndMarkerLine Nothing:_) = return []
extractSnipContent lbl (EndMarkerLine (Just endLbl):rest)
  | lbl == toFilePath endLbl = return []
  | otherwise = extractSnipContent lbl rest
extractSnipContent lbl (StartMarkerLine _:rest) = extractSnipContent lbl rest
extractSnipContent lbl parsedLines@(ContentLine _:_) = do
    App{appDoTrim} <- ask
    (contentLines appDoTrim <>) <$> extractSnipContent lbl rest
  where
    contentLines doTrim = reduceContent doTrim commonLeadingWS <$> contentSpecs
    reduceContent False _ (lineLeadWS, content) = expandWS lineLeadWS <> content
    reduceContent True comLeadWS (lineLeadWS, content) =
      case (comLeadWS, lineLeadWS) of
        (_,NoLeadingWS) -> content
        (NoLeadingWS,_) -> expandWS lineLeadWS <> content
        (LeadingSpaces _, LeadingTabs _) -> content
        (LeadingTabs _, LeadingSpaces _) -> content
        (LeadingSpaces comCnt, LeadingSpaces lineCnt) ->
          expandWS (LeadingSpaces $ lineCnt - min lineCnt comCnt) <> content
        (LeadingTabs comCnt, LeadingTabs lineCnt) ->
          expandWS (LeadingTabs $ lineCnt - min lineCnt comCnt) <> content
    expandWS = \case
      NoLeadingWS -> ""
      LeadingSpaces cnt -> T.pack $ L.replicate (fromIntegral cnt) ' '
      LeadingTabs cnt -> T.pack $ L.replicate (fromIntegral cnt) '\t'
    commonLeadingWS = fromMaybe NoLeadingWS $
      foldr 
        (\item memo ->
          case (item,memo) of
            ((NoLeadingWS,content), _) -> 
              if T.null content then
                memo
              else 
                Just NoLeadingWS
            (_, Just NoLeadingWS) -> Just NoLeadingWS
            ((lead,_), Nothing) -> Just lead
            ((LeadingTabs lineCnt,_), Just (LeadingTabs comCnt)) ->
              Just (LeadingTabs $ min lineCnt comCnt)
            ((LeadingSpaces lineCnt,_), Just (LeadingSpaces comCnt)) ->
              Just (LeadingSpaces $ min lineCnt comCnt)
            ((LeadingTabs _,_), Just (LeadingSpaces _)) ->
              Just NoLeadingWS
            ((LeadingSpaces _,_), Just (LeadingTabs _)) ->
              Just NoLeadingWS
        )
        Nothing
        contentSpecs
    contentSpecs = catMaybes $ mayContent <$> contentParsedLines
    (contentParsedLines, rest) = L.span isContentLine parsedLines
    mayContent = \case
        EmptyLine -> Just (NoLeadingWS, "")
        ContentLine spec -> Just spec
        _ -> Nothing
    isContentLine = isJust . mayContent
