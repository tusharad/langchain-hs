{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Description : AegisCode AI entry point with CLI argument parsing
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Application entry point with CLI argument parsing for three modes:
  * @serve@ — starts REST + WebSocket server
  * @scan@  — runs single scan pipeline from CLI
  * @demo@  — runs with MockModel for showcase/testing
-}
module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Exit (exitSuccess, exitFailure)

import Aegis.Core.Types.Config
import Aegis.Core.Types.Pipeline (phaseToText, statePhase, stateReport)
import Aegis.Graph.Pipeline (runPipeline, runDemoPipeline, buildPipeline, PipelineResult (..))
import Aegis.Server.Server (startServer)

-- ---------------------------------------------------------------------------
-- CLI Options
-- ---------------------------------------------------------------------------

-- | Top-level CLI command
data Command
  = Serve ServeOpts
  | Scan ScanOpts
  | Demo DemoOpts
  deriving (Show)

-- | Options for the 'serve' command
data ServeOpts = ServeOpts
  { servePort :: Int
  , serveHost :: Text
  }
  deriving (Show)

-- | Options for the 'scan' command
data ScanOpts = ScanOpts
  { scanRepoPath :: FilePath
  , scanExtensions :: [Text]
  , scanMaxFindings :: Int
  , scanHITL :: Bool
  }
  deriving (Show)

-- | Options for the 'demo' command
data DemoOpts = DemoOpts
  { demoRepoPath :: Maybe FilePath
  }
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

-- | Parse serve command options
serveParser :: Parser Command
serveParser = Serve <$> (ServeOpts
  <$> option auto
      ( long "port"
     <> short 'p'
     <> metavar "PORT"
     <> value 8080
     <> showDefault
     <> help "Port to listen on" )
  <*> strOption
      ( long "host"
     <> short 'h'
     <> metavar "HOST"
     <> value "127.0.0.1"
     <> showDefault
     <> help "Host to bind to" ))

-- | Parse scan command options
scanParser :: Parser Command
scanParser = Scan <$> (ScanOpts
  <$> strOption
      ( long "repo"
     <> short 'r'
     <> metavar "PATH"
     <> help "Path to the repository to scan" )
  <*> many (strOption
      ( long "ext"
     <> short 'e'
     <> metavar "EXT"
     <> help "File extension to include (e.g., .hs)" ))
  <*> option auto
      ( long "max-findings"
     <> short 'm'
     <> metavar "N"
     <> value 10
     <> showDefault
     <> help "Maximum number of findings to process" )
  <*> switch
      ( long "hitl"
     <> help "Enable human-in-the-loop approval" ))

-- | Parse demo command options
demoParser :: Parser Command
demoParser = Demo <$> (DemoOpts
  <$> optional (strOption
      ( long "repo"
     <> short 'r'
     <> metavar "PATH"
     <> help "Optional repository path (defaults to current directory)" )))

-- | Top-level command parser
commandParser :: Parser Command
commandParser = hsubparser
  ( command "serve"
      (info serveParser
        (progDesc "Start the REST + WebSocket server"))
  <> command "scan"
      (info scanParser
        (progDesc "Run a single security scan on a repository"))
  <> command "demo"
      (info demoParser
        (progDesc "Run a demo scan with MockModel (no external dependencies)"))
  )

-- | Full option parser with program info
opts :: ParserInfo Command
opts = info (commandParser <**> helper)
  ( fullDesc
  <> progDesc "AegisCode AI — Enterprise Autonomous Software Security Engine"
  <> header "aegis-code-ai - security analysis and refactoring orchestrator" )

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Serve serveOpts -> runServe serveOpts
    Scan scanOpts   -> runScan scanOpts
    Demo demoOpts   -> runDemo demoOpts

-- | Run the server
runServe :: ServeOpts -> IO ()
runServe opts_ = do
  TIO.putStrLn "Starting AegisCode AI server..."
  let config = defaultAegisConfig
        { configServer = (configServer defaultAegisConfig)
            { serverPort = servePort opts_
            , serverHost = serveHost opts_
            }
        }
  startServer config

-- | Run a single scan
runScan :: ScanOpts -> IO ()
runScan opts_ = do
  TIO.putStrLn $ T.unlines
    [ "╔══════════════════════════════════════════╗"
    , "║     AegisCode AI — Security Scanner      ║"
    , "╚══════════════════════════════════════════╝"
    , ""
    , "Repository: " <> T.pack (scanRepoPath opts_)
    , "Max findings: " <> T.pack (show (scanMaxFindings opts_))
    , "HITL: " <> if scanHITL opts_ then "enabled" else "disabled"
    , ""
    ]

  let config = defaultAegisConfig
        { configRepository = (configRepository defaultAegisConfig)
            { repoPath = scanRepoPath opts_
            , repoTargetExtensions = if null (scanExtensions opts_)
                then repoTargetExtensions (configRepository defaultAegisConfig)
                else scanExtensions opts_
            }
        , configScan = (configScan defaultAegisConfig)
            { scanMaxVulnerabilities = scanMaxFindings opts_
            , scanRequireHITL = scanHITL opts_
            }
        }

  ctx <- buildPipeline config
  result <- runPipeline ctx (scanRepoPath opts_)

  TIO.putStrLn $ T.unlines
    [ ""
    , "=== Scan Complete ==="
    , "Final phase: " <> phaseToText (statePhase (prFinalState result))
    , "Total iterations: " <> T.pack (show (prTotalIterations result))
    , "Tokens used: " <> T.pack (show (prTotalTokensUsed result))
    ]

  case prReport result of
    Nothing -> TIO.putStrLn "No report generated."
    Just report -> TIO.putStrLn $ "Report: " <> T.pack (show report)

-- | Run a demo scan
runDemo :: DemoOpts -> IO ()
runDemo opts_ = do
  let repoPath = maybe "." id (demoRepoPath opts_)

  TIO.putStrLn $ T.unlines
    [ "╔══════════════════════════════════════════╗"
    , "║    AegisCode AI — Demo Mode (MockLLM)    ║"
    , "╚══════════════════════════════════════════╝"
    , ""
    , "Running demo pipeline with mock LLM responses..."
    , "Repository: " <> T.pack repoPath
    , ""
    ]

  result <- runDemoPipeline repoPath

  TIO.putStrLn $ T.unlines
    [ ""
    , "=== Demo Complete ==="
    , "Final phase: " <> phaseToText (statePhase (prFinalState result))
    , "Total iterations: " <> T.pack (show (prTotalIterations result))
    , "Tokens used: " <> T.pack (show (prTotalTokensUsed result))
    , ""
    , "Demo finished successfully!"
    , "To run a real scan: aegis-code-ai scan --repo /path/to/repo"
    , "To start the server: aegis-code-ai serve --port 8080"
    ]
