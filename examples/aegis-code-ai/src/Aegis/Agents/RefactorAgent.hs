{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.Agents.RefactorAgent
Description : Patch generation and code refactoring agent
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

StateGraph worker node that generates unified diff patches to remediate
identified vulnerabilities. Uses the HybridRetriever to fetch context
across affected files and dependencies, constructs detailed prompts
with vulnerability context, and emits PatchDiff candidates into AegisState.
-}
module Aegis.Agents.RefactorAgent
  ( -- * Agent
    runRefactorAgent
  , refactorNode

    -- * Configuration
  , RefactorAgentConfig (..)
  , defaultRefactorConfig

    -- * Patch Generation
  , parsePatchOutput
  , buildRefactorPrompt
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)

import Langchain.Core.Model.Types (Message, systemMessage, userMessage, assistantMessage, extractMessageText)

import Aegis.Core.Types.Pipeline
import Aegis.Core.Types.Security
import Aegis.Core.Types.AST (CodeChunk (..), chunkIdentifier)
import Aegis.Middleware.Telemetry (TelemetrySystem, emitAgentStart, emitAgentEnd, emitInfo)
import Aegis.RAG.HybridRetriever (HybridRetriever, hybridRetrieve, RetrievalResult (..))

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Configuration for the refactor agent
data RefactorAgentConfig = RefactorAgentConfig
  { raMaxContextChunks :: Int
  -- ^ Maximum number of code chunks to include in context
  , raMaxPatchSize :: Int
  -- ^ Maximum number of lines in a single patch
  , raPreserveComments :: Bool
  -- ^ Whether to preserve existing comments
  , raPreserveFormatting :: Bool
  -- ^ Whether to preserve existing code formatting style
  , raSystemPrompt :: Text
  -- ^ System prompt for the refactoring LLM
  }
  deriving (Eq, Show)

-- | Default refactor configuration
defaultRefactorConfig :: RefactorAgentConfig
defaultRefactorConfig = RefactorAgentConfig
  { raMaxContextChunks = 15
  , raMaxPatchSize = 200
  , raPreserveComments = True
  , raPreserveFormatting = True
  , raSystemPrompt = refactorSystemPrompt
  }

-- ---------------------------------------------------------------------------
-- System Prompt
-- ---------------------------------------------------------------------------

refactorSystemPrompt :: Text
refactorSystemPrompt = T.unlines
  [ "You are the AegisCode AI Refactoring Agent, an expert Haskell developer and security engineer."
  , "Your role is to generate precise, minimal unified diff patches to fix identified vulnerabilities."
  , ""
  , "RULES:"
  , "1. Generate ONLY valid unified diff format output"
  , "2. Make MINIMAL changes - fix the vulnerability without restructuring unrelated code"
  , "3. PRESERVE all existing comments, docstrings, and formatting"
  , "4. Ensure the patch maintains type safety and does not introduce new warnings"
  , "5. If the fix requires importing new modules, include those changes"
  , "6. Test your mental model of the code before generating the patch"
  , ""
  , "OUTPUT FORMAT:"
  , "```diff"
  , "--- a/<file>"
  , "+++ b/<file>"
  , "@@ -<old_start>,<old_count> +<new_start>,<new_count> @@"
  , " <context line>"
  , "-<removed line>"
  , "+<added line>"
  , " <context line>"
  , "```"
  , ""
  , "After the diff, provide:"
  , "DESCRIPTION: <brief description of the change>"
  , "RISK: <LOW|MEDIUM|HIGH>"
  ]

-- ---------------------------------------------------------------------------
-- Agent Execution
-- ---------------------------------------------------------------------------

-- | Run the refactor agent for a single vulnerability
runRefactorAgent
  :: RefactorAgentConfig
  -> Maybe TelemetrySystem
  -> Maybe HybridRetriever
  -> VulnerabilityTarget
  -> [CodeChunk]       -- ^ Relevant code chunks (pre-fetched or from RAG)
  -> (Text -> IO Text) -- ^ LLM invocation function
  -> IO (Maybe PatchDiff, [Message])
runRefactorAgent config mbTelemetry mbRetriever vuln chunks invokeLLM = do
  mapM_ (\ts -> emitAgentStart ts "RefactorAgent") mbTelemetry

  -- Step 1: Fetch additional context via RAG if available
  ragChunks <- case mbRetriever of
    Nothing -> pure []
    Just retriever -> do
      results <- hybridRetrieve retriever (vtTitle vuln <> " " <> vtDescription vuln)
      pure $ map rrChunk (take (raMaxContextChunks config) results)

  let allChunks = chunks ++ ragChunks

  -- Step 2: Build the refactoring prompt
  let prompt = buildRefactorPrompt config vuln allChunks

  -- Step 3: Invoke LLM for patch generation
  llmResponse <- invokeLLM prompt

  -- Step 4: Parse the patch from the response
  let mbPatch = parsePatchOutput (locFilePath (vtLocation vuln)) (vtId vuln) llmResponse

  let responseMsg = assistantMessage $ case mbPatch of
        Nothing -> "Failed to generate a valid patch for: " <> vtTitle vuln
        Just patch -> "Generated patch for " <> vtTitle vuln <> ":\n" <> patchRawDiff patch

  mapM_ (\ts -> emitAgentEnd ts "RefactorAgent" 0.0) mbTelemetry

  pure (mbPatch, [responseMsg])

-- | StateGraph node wrapper for the refactor agent
refactorNode
  :: RefactorAgentConfig
  -> Maybe TelemetrySystem
  -> Maybe HybridRetriever
  -> (Text -> IO Text) -- ^ LLM invocation function
  -> AegisState
  -> IO AegisState
refactorNode config mbTelemetry mbRetriever invokeLLM state = do
  mapM_ (\ts -> emitInfo ts "RefactorAgent" "Starting patch generation...") mbTelemetry

  case stateCurrentVulnerability state of
    Nothing -> do
      -- Pick the next vulnerability from the queue
      case stateVulnerabilities state of
        [] -> pure state
          { statePhase = PhaseReporting
          , stateEventLog = stateEventLog state ++
              [logEvent PhaseRefactoring "RefactorAgent" "No vulnerabilities to process" EventInfo]
          }
        (vuln : rest) -> do
          -- Process this vulnerability
          (mbPatch, newMsgs) <- runRefactorAgent config mbTelemetry mbRetriever vuln (stateCodeChunks state) invokeLLM
          pure state
            { statePhase = PhaseRefactoring
            , stateCurrentVulnerability = Just vuln
            , stateCurrentPatch = mbPatch
            , statePatches = maybe (statePatches state) (\p -> statePatches state ++ [p]) mbPatch
            , stateVulnerabilities = rest
            , stateMessages = newMsgs
            , stateEventLog = stateEventLog state ++
                [logEvent PhaseRefactoring "RefactorAgent"
                  (case mbPatch of
                    Nothing -> "Failed to generate patch for: " <> vtTitle vuln
                    Just _ -> "Generated patch for: " <> vtTitle vuln) EventInfo]
            , stateIterationCount = stateIterationCount state + 1
            }

    Just vuln -> do
      -- Re-attempt for current vulnerability (retry scenario)
      (mbPatch, newMsgs) <- runRefactorAgent config mbTelemetry mbRetriever vuln (stateCodeChunks state) invokeLLM
      pure state
        { statePhase = PhaseRefactoring
        , stateCurrentPatch = mbPatch
        , statePatches = maybe (statePatches state) (\p -> statePatches state ++ [p]) mbPatch
        , stateMessages = newMsgs
        , stateIterationCount = stateIterationCount state + 1
        }

-- ---------------------------------------------------------------------------
-- Prompt Construction
-- ---------------------------------------------------------------------------

-- | Build a comprehensive refactoring prompt with vulnerability context
buildRefactorPrompt :: RefactorAgentConfig -> VulnerabilityTarget -> [CodeChunk] -> Text
buildRefactorPrompt config vuln chunks = T.unlines
  [ raSystemPrompt config
  , ""
  , "=== VULNERABILITY TO FIX ==="
  , "Title: " <> vtTitle vuln
  , "Severity: " <> severityToText (vtSeverity vuln)
  , "Category: " <> T.pack (show (vtCategory vuln))
  , "File: " <> T.pack (locFilePath (vtLocation vuln))
  , "Description: " <> vtDescription vuln
  , ""
  , case vtHints vuln of
      [] -> ""
      hints -> "Remediation Hints:\n" <> T.unlines (map (\h -> "  - " <> hintDescription h) hints)
  , ""
  , "=== RELEVANT CODE CONTEXT ==="
  , T.unlines $ map formatChunkContext (take (raMaxContextChunks config) chunks)
  , ""
  , "Generate a unified diff patch to fix this vulnerability."
  , "The patch should be minimal and precise."
  ]

-- | Format a code chunk for inclusion in the prompt
formatChunkContext :: CodeChunk -> Text
formatChunkContext chunk = T.unlines
  [ "--- " <> T.pack (chunkFilePath chunk) <> " (lines "
    <> T.pack (show (chunkStartLine chunk)) <> "-"
    <> T.pack (show (chunkEndLine chunk)) <> ") ---"
  , maybe "" (\sig -> "Signature: " <> sig) (chunkSignature chunk)
  , chunkContent chunk
  ]

-- ---------------------------------------------------------------------------
-- Patch Parsing
-- ---------------------------------------------------------------------------

-- | Parse a unified diff patch from LLM output
parsePatchOutput :: FilePath -> Text -> Text -> Maybe PatchDiff
parsePatchOutput defaultFile vulnId response =
  let diffBlocks = extractDiffBlocks response
  in case diffBlocks of
       [] -> Nothing
       (diffText : _) ->
         let description = extractDescription response
             (added, removed) = countDiffLines diffText
         in Just PatchDiff
              { patchFilePath = extractPatchFile diffText defaultFile
              , patchOldPath = Nothing
              , patchHunks = parseHunks diffText
              , patchRawDiff = diffText
              , patchDescription = description
              , patchVulnerabilityId = Just vulnId
              , patchLinesAdded = added
              , patchLinesRemoved = removed
              }

-- | Extract diff blocks from markdown-formatted LLM output
extractDiffBlocks :: Text -> [Text]
extractDiffBlocks response =
  let segments = T.splitOn "```diff" response
  in [T.takeWhile (/= '`') content | content <- drop 1 segments
     , let afterTick = T.strip content
     , not (T.null afterTick)]
  -- Also try without the language marker
  ++ [T.takeWhile (/= '`') content
     | T.isInfixOf "--- a/" response || T.isInfixOf "@@" response
     , let content = response
     , T.null content -- This branch intentionally doesn't produce results; it's a fallback placeholder
     ]

-- | Extract the file path from a diff
extractPatchFile :: Text -> FilePath -> FilePath
extractPatchFile diffText defaultFile =
  case filter ("--- a/" `T.isPrefixOf`) (T.lines diffText) of
    (l : _) -> T.unpack $ T.strip $ T.drop 6 l
    _ -> case filter ("+++ b/" `T.isPrefixOf`) (T.lines diffText) of
      (l : _) -> T.unpack $ T.strip $ T.drop 6 l
      _ -> defaultFile

-- | Parse diff hunks
parseHunks :: Text -> [DiffHunk]
parseHunks diffText =
  let ls = T.lines diffText
      hunkStarts = [i | (i, l) <- zip [0..] ls, "@@" `T.isPrefixOf` l]
  in map (parseHunkAt ls) hunkStarts

parseHunkAt :: [Text] -> Int -> DiffHunk
parseHunkAt ls idx =
  let headerLine = ls !! idx
      (oldStart, oldCount, newStart, newCount) = parseHunkHeader headerLine
      bodyLines = takeWhile (not . ("@@" `T.isPrefixOf`)) (drop (idx + 1) ls)
  in DiffHunk oldStart oldCount newStart newCount (T.unlines bodyLines)

-- | Parse a hunk header like "@@ -10,5 +10,7 @@"
parseHunkHeader :: Text -> (Int, Int, Int, Int)
parseHunkHeader header =
  let stripped = T.strip $ T.takeWhile (/= '@') $ T.drop 3 header
      parts = T.words stripped
  in case parts of
       [oldPart, newPart] ->
         let (os, oc) = parseRange (T.drop 1 oldPart)
             (ns, nc) = parseRange (T.drop 1 newPart)
         in (os, oc, ns, nc)
       _ -> (0, 0, 0, 0)
  where
    parseRange :: Text -> (Int, Int)
    parseRange t = case T.splitOn "," t of
      [s, c] -> (readInt s, readInt c)
      [s]    -> (readInt s, 1)
      _      -> (0, 0)

    readInt :: Text -> Int
    readInt txt = case reads (T.unpack txt) :: [(Int, String)] of
      [(n, _)] -> n
      _ -> 0

-- | Count added and removed lines in a diff
countDiffLines :: Text -> (Int, Int)
countDiffLines diffText =
  let ls = T.lines diffText
      added = length [l | l <- ls, "+" `T.isPrefixOf` l, not ("++" `T.isPrefixOf` l)]
      removed = length [l | l <- ls, "-" `T.isPrefixOf` l, not ("--" `T.isPrefixOf` l)]
  in (added, removed)

-- | Extract description from LLM response
extractDescription :: Text -> Text
extractDescription response =
  case filter ("DESCRIPTION:" `T.isPrefixOf`) (T.lines response) of
    (l : _) -> T.strip $ T.drop 12 l
    _ -> "Auto-generated patch"
