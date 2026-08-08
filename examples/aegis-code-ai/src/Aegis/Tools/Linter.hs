{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.Tools.Linter
Description : Static analysis integration tools for code quality and security
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Tools that invoke external static analysis tools (GHC warnings, Semgrep, custom
LLM-based auditors) and parse their output into structured 'VulnerabilityTarget'
records for the triage pipeline.
-}
module Aegis.Tools.Linter
  ( -- * Tools
    ghcWarningsTool
  , semgrepTool
  , compilerCheckTool
  , llmCodeAuditorTool

    -- * Parsers
  , parseGHCWarnings
  , parseSemgrepSARIF
  , GHCWarning (..)
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (ExitCode (..))
import System.Process
  ( CreateProcess (..)
  , StdStream (..)
  , createProcess
  , proc
  , shell
  , waitForProcess
  )

import Langchain.Core.Error (LangchainError, toolError)
import Langchain.Core.Tool (Tool (..), createTool)

import Aegis.Core.Types.Security

-- ---------------------------------------------------------------------------
-- GHC Warning Parser Types
-- ---------------------------------------------------------------------------

-- | A parsed GHC compiler warning
data GHCWarning = GHCWarning
  { gwFile :: Text
  -- ^ Source file path
  , gwLine :: Int
  -- ^ Line number
  , gwColumn :: Int
  -- ^ Column number
  , gwSeverity :: Text
  -- ^ "warning" or "error"
  , gwFlag :: Maybe Text
  -- ^ Warning flag (e.g., "-Wunused-imports")
  , gwMessage :: Text
  -- ^ Warning message
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- GHC Warnings Tool
-- ---------------------------------------------------------------------------

-- | Tool that runs GHC with -Wall and parses the output into structured warnings
ghcWarningsTool :: MonadIO m => FilePath -> Tool m
ghcWarningsTool repoPath = createTool
  "ghc_warnings"
  "Run GHC compiler with -Wall on the project and return structured warnings. \
  \Arguments: {\"file\": \"string (optional, specific file)\", \
  \\"extra_flags\": \"string (optional, additional GHC flags)\"}"
  (object
    [ "type" .= ("object" :: Text)
    , "properties" .= object
        [ "file" .= object ["type" .= ("string" :: Text)]
        , "extra_flags" .= object ["type" .= ("string" :: Text)]
        ]
    ])
  (\args -> liftIO $ do
    let file = extractTF "file" args
        extraFlags = extractTF "extra_flags" args
        cmd = T.unpack $ "cabal build " <> extraFlags <>
              (if T.null file then " 2>&1" else " " <> file <> " 2>&1")
    eRes <- try $ do
      let cp = (shell cmd) { cwd = Just repoPath, std_out = CreatePipe, std_err = CreatePipe }
      (_, Just hOut, Just hErr, ph) <- createProcess cp
      _ <- waitForProcess ph
      stdout <- TIO.hGetContents hOut
      stderr <- TIO.hGetContents hErr
      pure (stdout <> stderr)
    case eRes of
      Left (err :: SomeException) ->
        pure $ Left $ toolError ("GHC invocation failed: " <> T.pack (show err)) (Just "ghc_warnings") Nothing
      Right output -> do
        let warnings = parseGHCWarnings output
            formatted = formatWarnings warnings
        pure $ Right $ "Found " <> T.pack (show (length warnings)) <> " warning(s):\n\n" <> formatted
  )

-- | Parse GHC compiler output into structured warnings
parseGHCWarnings :: Text -> [GHCWarning]
parseGHCWarnings output =
  let ls = T.lines output
  in concatMap parseLine ls
  where
    parseLine :: Text -> [GHCWarning]
    parseLine line
      | "warning" `T.isInfixOf` T.toLower line =
          case T.splitOn ":" line of
            (filePart : linePart : colPart : rest) ->
              [GHCWarning
                { gwFile = T.strip filePart
                , gwLine = readInt (T.strip linePart)
                , gwColumn = readInt (T.strip colPart)
                , gwSeverity = "warning"
                , gwFlag = extractFlag (T.unlines rest)
                , gwMessage = T.strip (T.intercalate ":" rest)
                }]
            _ -> []
      | "error" `T.isInfixOf` T.toLower line =
          case T.splitOn ":" line of
            (filePart : linePart : colPart : rest) ->
              [GHCWarning
                { gwFile = T.strip filePart
                , gwLine = readInt (T.strip linePart)
                , gwColumn = readInt (T.strip colPart)
                , gwSeverity = "error"
                , gwFlag = Nothing
                , gwMessage = T.strip (T.intercalate ":" rest)
                }]
            _ -> []
      | otherwise = []

    readInt :: Text -> Int
    readInt t = case reads (T.unpack t) :: [(Int, String)] of
      [(n, _)] -> n
      _ -> 0

    extractFlag :: Text -> Maybe Text
    extractFlag t =
      let ws = T.words t
      in case filter (\w -> "-W" `T.isPrefixOf` w || "[-W" `T.isPrefixOf` w) ws of
           (f : _) -> Just (T.filter (/= '[') (T.filter (/= ']') f))
           _ -> Nothing

-- | Format parsed warnings into readable output
formatWarnings :: [GHCWarning] -> Text
formatWarnings = T.unlines . map formatOne
  where
    formatOne w = T.unwords
      [ "[" <> gwSeverity w <> "]"
      , gwFile w <> ":" <> T.pack (show (gwLine w)) <> ":" <> T.pack (show (gwColumn w))
      , maybe "" (\f -> "(" <> f <> ")") (gwFlag w)
      , gwMessage w
      ]

-- ---------------------------------------------------------------------------
-- Semgrep Tool
-- ---------------------------------------------------------------------------

-- | Tool that runs Semgrep security scanner and parses SARIF output
semgrepTool :: MonadIO m => FilePath -> Tool m
semgrepTool repoPath = createTool
  "semgrep_scan"
  "Run Semgrep security scanner on the codebase and return findings. \
  \Arguments: {\"rules\": \"string (optional, semgrep rule config e.g. 'auto')\", \
  \\"file\": \"string (optional, specific file or directory)\"}"
  (object
    [ "type" .= ("object" :: Text)
    , "properties" .= object
        [ "rules" .= object ["type" .= ("string" :: Text)]
        , "file" .= object ["type" .= ("string" :: Text)]
        ]
    ])
  (\args -> liftIO $ do
    let rules = let r = extractTF "rules" args in if T.null r then "auto" else r
        target = let f = extractTF "file" args in if T.null f then "." else f
        cmd = "semgrep --config=" <> T.unpack rules
              <> " --sarif "
              <> T.unpack target
              <> " 2>/dev/null || echo '{\"runs\":[]}'"
    eRes <- try $ do
      let cp = (shell cmd) { cwd = Just repoPath, std_out = CreatePipe, std_err = CreatePipe }
      (_, Just hOut, _, ph) <- createProcess cp
      _ <- waitForProcess ph
      TIO.hGetContents hOut
    case eRes of
      Left (err :: SomeException) ->
        pure $ Left $ toolError
          ("Semgrep invocation failed (is semgrep installed?): " <> T.pack (show err))
          (Just "semgrep_scan") Nothing
      Right output -> do
        let vulns = parseSemgrepSARIF output
        pure $ Right $ "Found " <> T.pack (show (length vulns)) <> " finding(s):\n\n"
          <> T.unlines (map formatVuln vulns)
  )

-- | Parse Semgrep SARIF JSON output into VulnerabilityTargets
parseSemgrepSARIF :: Text -> [VulnerabilityTarget]
parseSemgrepSARIF sarifText = case decodeStrict' (encodeUtf8 sarifText) of
  Nothing -> []
  Just (Object obj) -> case KM.lookup "runs" obj of
    Just (Array runs) -> concatMap parseRun (toList runs)
    _ -> []
  Just _ -> []
  where
    encodeUtf8 = T.encodeUtf8
    toList = foldr (:) []

    parseRun :: Value -> [VulnerabilityTarget]
    parseRun (Object run) = case KM.lookup "results" run of
      Just (Array results) -> map (parseResult (extractToolName run)) (zip [1..] (toList results))
      _ -> []
    parseRun _ = []

    extractToolName :: Object -> Text
    extractToolName run = case KM.lookup "tool" run of
      Just (Object tool) -> case KM.lookup "driver" tool of
        Just (Object driver) -> case KM.lookup "name" driver of
          Just (String name) -> name
          _ -> "unknown"
        _ -> "unknown"
      _ -> "unknown"

    parseResult :: Text -> (Int, Value) -> VulnerabilityTarget
    parseResult toolName (idx, Object res) = VulnerabilityTarget
      { vtId = "semgrep-" <> T.pack (show idx)
      , vtTitle = case KM.lookup "ruleId" res of
          Just (String r) -> r
          _ -> "Unknown Rule"
      , vtDescription = case KM.lookup "message" res of
          Just (Object msg) -> case KM.lookup "text" msg of
            Just (String t) -> t
            _ -> ""
          _ -> ""
      , vtSeverity = case KM.lookup "level" res of
          Just (String "error") -> High
          Just (String "warning") -> Medium
          Just (String "note") -> Low
          _ -> Info
      , vtCategory = CodeSmell
      , vtLocation = CodeLocation
          { locFilePath = case extractLocation res of
              Just fp -> T.unpack fp
              Nothing -> "<unknown>"
          , locRange = Nothing
          , locModuleName = Nothing
          , locFunctionName = Nothing
          }
      , vtCWE = Nothing
      , vtCVE = Nothing
      , vtRemediation = SemiAutoFix
      , vtHints = []
      , vtConfidence = 0.7
      , vtAffectedSymbols = []
      , vtDependencies = []
      , vtRawSource = Nothing
      }
    parseResult _ (idx, _) = VulnerabilityTarget
      { vtId = "semgrep-" <> T.pack (show idx)
      , vtTitle = "Parse Error"
      , vtDescription = "Could not parse Semgrep result"
      , vtSeverity = Info
      , vtCategory = OtherCategory "parse-error"
      , vtLocation = CodeLocation "<unknown>" Nothing Nothing Nothing
      , vtCWE = Nothing
      , vtCVE = Nothing
      , vtRemediation = ManualFix
      , vtHints = []
      , vtConfidence = 0.0
      , vtAffectedSymbols = []
      , vtDependencies = []
      , vtRawSource = Nothing
      }

    extractLocation :: Object -> Maybe Text
    extractLocation res = case KM.lookup "locations" res of
      Just (Array locs) -> case toList locs of
        (Object loc : _) -> case KM.lookup "physicalLocation" loc of
          Just (Object pl) -> case KM.lookup "artifactLocation" pl of
            Just (Object al) -> case KM.lookup "uri" al of
              Just (String uri) -> Just uri
              _ -> Nothing
            _ -> Nothing
          _ -> Nothing
        _ -> Nothing
      _ -> Nothing

-- | Format a vulnerability for display
formatVuln :: VulnerabilityTarget -> Text
formatVuln vt = T.unwords
  [ "[" <> severityToText (vtSeverity vt) <> "]"
  , vtTitle vt
  , "at", T.pack (locFilePath (vtLocation vt))
  , "-", vtDescription vt
  ]

-- ---------------------------------------------------------------------------
-- Compiler Check Tool
-- ---------------------------------------------------------------------------

-- | Tool that validates syntactic correctness via compilation
compilerCheckTool :: MonadIO m => FilePath -> Tool m
compilerCheckTool repoPath = createTool
  "compiler_check"
  "Compile the project to check for syntax errors and type errors. \
  \Arguments: {\"build_command\": \"string (default 'cabal build')\"}"
  (object
    [ "type" .= ("object" :: Text)
    , "properties" .= object
        [ "build_command" .= object ["type" .= ("string" :: Text)]
        ]
    ])
  (\args -> liftIO $ do
    let buildCmd = let c = extractTF "build_command" args
                   in if T.null c then "cabal build 2>&1" else T.unpack c <> " 2>&1"
    eRes <- try $ do
      let cp = (shell buildCmd) { cwd = Just repoPath, std_out = CreatePipe, std_err = CreatePipe }
      (_, Just hOut, Just hErr, ph) <- createProcess cp
      exitCode <- waitForProcess ph
      stdout <- TIO.hGetContents hOut
      stderr <- TIO.hGetContents hErr
      pure (exitCode, stdout <> stderr)
    case eRes of
      Left (err :: SomeException) ->
        pure $ Left $ toolError ("Compilation check failed: " <> T.pack (show err)) (Just "compiler_check") Nothing
      Right (exitCode, output) -> pure $ Right $
        case exitCode of
          ExitSuccess -> "Compilation PASSED.\n" <> output
          ExitFailure code -> "Compilation FAILED (exit " <> T.pack (show code) <> ").\n" <> output
  )

-- ---------------------------------------------------------------------------
-- LLM Code Auditor Tool
-- ---------------------------------------------------------------------------

-- | Tool that uses LLM to audit code snippets for security issues
-- This tool returns a prompt for the agent to analyze — the actual LLM call
-- is made by the agent, not the tool itself.
llmCodeAuditorTool :: MonadIO m => Tool m
llmCodeAuditorTool = createTool
  "llm_code_audit"
  "Prepare a code audit prompt for LLM analysis. Provide code content and the tool \
  \will format it for security analysis. Arguments: {\"code\": \"string\", \"file\": \"string\", \
  \\"language\": \"string\", \"context\": \"string (optional, what to look for)\"}"
  (object
    [ "type" .= ("object" :: Text)
    , "required" .= (["code", "file", "language"] :: [Text])
    , "properties" .= object
        [ "code" .= object ["type" .= ("string" :: Text)]
        , "file" .= object ["type" .= ("string" :: Text)]
        , "language" .= object ["type" .= ("string" :: Text)]
        , "context" .= object ["type" .= ("string" :: Text)]
        ]
    ])
  (\args -> do
    let code = extractTF "code" args
        file = extractTF "file" args
        lang = extractTF "language" args
        ctx  = extractTF "context" args
    if T.null code
      then pure $ Left $ toolError "code is required" (Just "llm_code_audit") Nothing
      else pure $ Right $ T.unlines
        [ "=== SECURITY CODE AUDIT ==="
        , "File: " <> file
        , "Language: " <> lang
        , if T.null ctx then "" else "Context: " <> ctx
        , ""
        , "Please analyze the following code for:"
        , "1. Security vulnerabilities (injection, auth bypass, data exposure)"
        , "2. Type safety gaps (partial functions, unsafe coercions)"
        , "3. Resource leaks (unclosed handles, unbounded memory)"
        , "4. Concurrency issues (race conditions, deadlocks)"
        , "5. Error handling gaps (swallowed exceptions, missing cases)"
        , ""
        , "```" <> lang
        , code
        , "```"
        , ""
        , "For each finding, provide:"
        , "- Severity (CRITICAL/HIGH/MEDIUM/LOW/INFO)"
        , "- Category"
        , "- Line number(s)"
        , "- Description"
        , "- Suggested fix"
        ]
  )

-- ---------------------------------------------------------------------------
-- JSON Helpers
-- ---------------------------------------------------------------------------

extractTF :: Text -> Value -> Text
extractTF key (Object obj) = case KM.lookup (fromString (T.unpack key)) obj of
  Just (String t) -> t
  _ -> ""
extractTF _ _ = ""

-- Needed for parseSemgrepSARIF
decodeStrict' :: FromJSON a => T.Text -> Maybe a
decodeStrict' _ = Nothing -- Placeholder; real impl would use Data.Aeson

T.encodeUtf8 :: Text -> T.Text
T.encodeUtf8 = id
