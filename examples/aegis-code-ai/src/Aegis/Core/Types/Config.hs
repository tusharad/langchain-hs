{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Aegis.Core.Types.Config
Description : System-wide configuration for AegisCode AI
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

System-wide configuration loaded from environment variables and configuration files.
Covers LLM provider credentials, token budgets, retry policies, repository paths,
Docker settings, and database connection strings.
-}
module Aegis.Core.Types.Config
  ( -- * Top-level Configuration
    AegisConfig (..)
  , defaultAegisConfig

    -- * LLM Provider Configuration
  , LLMProviderConfig (..)
  , OllamaProviderConfig (..)
  , defaultOllamaConfig

    -- * Token Budget Configuration
  , TokenBudgetConfig (..)
  , defaultTokenBudgetConfig

    -- * Retry & Circuit Breaker Configuration
  , RetryConfig (..)
  , defaultRetryConfig
  , CircuitBreakerConfig (..)
  , defaultCircuitBreakerConfig

    -- * Repository Configuration
  , RepositoryConfig (..)
  , defaultRepositoryConfig

    -- * Docker / Sandbox Configuration
  , SandboxConfig (..)
  , defaultSandboxConfig

    -- * Server Configuration
  , ServerConfig (..)
  , defaultServerConfig

    -- * Database Configuration
  , DatabaseConfig (..)
  , defaultDatabaseConfig

    -- * Scan Configuration
  , ScanConfig (..)
  , defaultScanConfig

    -- * Logging
  , LogLevel (..)
  , LogConfig (..)
  , defaultLogConfig
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- ---------------------------------------------------------------------------
-- Logging
-- ---------------------------------------------------------------------------

-- | Structured log levels for telemetry output
data LogLevel
  = LogDebug
  | LogInfo
  | LogWarn
  | LogError
  | LogFatal
  deriving (Eq, Ord, Show, Bounded, Enum, Generic, ToJSON, FromJSON)

-- | Logging configuration
data LogConfig = LogConfig
  { logLevel :: LogLevel
  -- ^ Minimum log level to emit
  , logStructured :: Bool
  -- ^ Whether to emit JSON structured logs
  , logFile :: Maybe FilePath
  -- ^ Optional file sink (in addition to stderr)
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Default logging: Info level, unstructured, stderr only
defaultLogConfig :: LogConfig
defaultLogConfig =
  LogConfig
    { logLevel = LogInfo
    , logStructured = False
    , logFile = Nothing
    }

-- ---------------------------------------------------------------------------
-- LLM Provider Configuration
-- ---------------------------------------------------------------------------

-- | Configuration for the Ollama LLM provider
data OllamaProviderConfig = OllamaProviderConfig
  { ollamaBaseUrl :: Text
  -- ^ Ollama server base URL
  , ollamaModel :: Text
  -- ^ Model name to use for code analysis
  , ollamaEmbeddingModel :: Text
  -- ^ Model name for embeddings generation
  , ollamaTimeoutSeconds :: Int
  -- ^ Request timeout in seconds
  , ollamaTemperature :: Double
  -- ^ Sampling temperature (0.0 = deterministic, 1.0 = creative)
  , ollamaMaxTokens :: Maybe Int
  -- ^ Optional maximum token limit for responses
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Default Ollama configuration pointing to localhost
defaultOllamaConfig :: OllamaProviderConfig
defaultOllamaConfig =
  OllamaProviderConfig
    { ollamaBaseUrl = "http://localhost:11434"
    , ollamaModel = "qwen3:4b"
    , ollamaEmbeddingModel = "nomic-embed-text:latest"
    , ollamaTimeoutSeconds = 120
    , ollamaTemperature = 0.1
    , ollamaMaxTokens = Nothing
    }

-- | Provider configuration wrapper (Ollama-only for this build)
data LLMProviderConfig = LLMProviderConfig
  { primaryProvider :: OllamaProviderConfig
  -- ^ Primary Ollama provider configuration
  , fallbackModels :: [Text]
  -- ^ List of fallback model names to try on failure
  , maxRetries :: Int
  -- ^ Maximum number of retries per provider before failing
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Token Budget
-- ---------------------------------------------------------------------------

-- | Token budgeting configuration to prevent runaway agent loops
data TokenBudgetConfig = TokenBudgetConfig
  { maxTotalTokens :: Int
  -- ^ Absolute ceiling on total tokens consumed per pipeline run
  , maxPromptTokens :: Int
  -- ^ Maximum tokens allowed in a single prompt
  , maxCompletionTokens :: Int
  -- ^ Maximum tokens for a single completion
  , warnThresholdPercent :: Double
  -- ^ Percentage of budget at which to emit warning events (0.0 to 1.0)
  , estimatedCostPerMillionTokens :: Double
  -- ^ Estimated cost per million tokens (for cost tracking/reporting)
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Default token budget: 500K total tokens per run, warn at 80%
defaultTokenBudgetConfig :: TokenBudgetConfig
defaultTokenBudgetConfig =
  TokenBudgetConfig
    { maxTotalTokens = 500000
    , maxPromptTokens = 32000
    , maxCompletionTokens = 8000
    , warnThresholdPercent = 0.80
    , estimatedCostPerMillionTokens = 0.0
    }

-- ---------------------------------------------------------------------------
-- Retry & Circuit Breaker
-- ---------------------------------------------------------------------------

-- | Retry policy configuration for transient failures
data RetryConfig = RetryConfig
  { retryMaxAttempts :: Int
  -- ^ Maximum number of retry attempts
  , retryBaseDelayMs :: Int
  -- ^ Base delay between retries in milliseconds
  , retryMaxDelayMs :: Int
  -- ^ Maximum backoff delay in milliseconds
  , retryExponentialBackoff :: Bool
  -- ^ Whether to use exponential backoff
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Default retry: 3 attempts, 500ms base delay, exponential backoff, 30s max
defaultRetryConfig :: RetryConfig
defaultRetryConfig =
  RetryConfig
    { retryMaxAttempts = 3
    , retryBaseDelayMs = 500
    , retryMaxDelayMs = 30000
    , retryExponentialBackoff = True
    }

-- | Circuit breaker configuration for provider health tracking
data CircuitBreakerConfig = CircuitBreakerConfig
  { cbFailureThreshold :: Int
  -- ^ Number of consecutive failures before opening circuit
  , cbResetTimeoutSeconds :: Int
  -- ^ Seconds to wait in open state before allowing probe request
  , cbHalfOpenMaxProbes :: Int
  -- ^ Number of probe requests in half-open state before deciding
  , cbSuccessThresholdToClose :: Int
  -- ^ Consecutive successes in half-open state to close circuit
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Default circuit breaker: open after 5 failures, reset after 60 seconds
defaultCircuitBreakerConfig :: CircuitBreakerConfig
defaultCircuitBreakerConfig =
  CircuitBreakerConfig
    { cbFailureThreshold = 5
    , cbResetTimeoutSeconds = 60
    , cbHalfOpenMaxProbes = 3
    , cbSuccessThresholdToClose = 2
    }

-- ---------------------------------------------------------------------------
-- Repository Configuration
-- ---------------------------------------------------------------------------

-- | Configuration for the target repository to analyze
data RepositoryConfig = RepositoryConfig
  { repoPath :: FilePath
  -- ^ Absolute path to the repository root
  , repoTargetExtensions :: [Text]
  -- ^ File extensions to include in analysis (e.g., [".hs", ".py"])
  , repoExcludePaths :: [Text]
  -- ^ Path patterns to exclude (e.g., [".git", "node_modules", "dist"])
  , repoMaxFileSizeBytes :: Int
  -- ^ Maximum file size to process (skip very large files)
  , repoGitBranch :: Maybe Text
  -- ^ Optional git branch to analyze (defaults to current)
  , repoRecursive :: Bool
  -- ^ Whether to recursively scan subdirectories
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Default repo config: all Haskell files, recursive, 1MB max file size
defaultRepositoryConfig :: RepositoryConfig
defaultRepositoryConfig =
  RepositoryConfig
    { repoPath = "."
    , repoTargetExtensions = [".hs"]
    , repoExcludePaths = [".git", ".stack-work", "dist-newstyle", "node_modules", ".cabal"]
    , repoMaxFileSizeBytes = 1048576
    , repoGitBranch = Nothing
    , repoRecursive = True
    }

-- ---------------------------------------------------------------------------
-- Sandbox Configuration
-- ---------------------------------------------------------------------------

-- | Configuration for sandboxed code execution (Docker-based)
data SandboxConfig = SandboxConfig
  { sandboxEnabled :: Bool
  -- ^ Whether sandboxed execution is enabled
  , sandboxDockerImage :: Text
  -- ^ Docker image to use for sandboxed execution
  , sandboxTimeoutSeconds :: Int
  -- ^ Maximum execution time in seconds per sandbox run
  , sandboxMemoryLimitMB :: Int
  -- ^ Memory limit for sandbox containers in MB
  , sandboxCpuLimit :: Double
  -- ^ CPU core limit (e.g., 1.0 = 1 core)
  , sandboxNetworkEnabled :: Bool
  -- ^ Whether network access is allowed inside sandbox
  , sandboxWorkDir :: FilePath
  -- ^ Working directory inside the container
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Default sandbox: disabled (Docker optional), conservative resource limits
defaultSandboxConfig :: SandboxConfig
defaultSandboxConfig =
  SandboxConfig
    { sandboxEnabled = False
    , sandboxDockerImage = "haskell:9.8-slim"
    , sandboxTimeoutSeconds = 300
    , sandboxMemoryLimitMB = 2048
    , sandboxCpuLimit = 2.0
    , sandboxNetworkEnabled = False
    , sandboxWorkDir = "/workspace"
    }

-- ---------------------------------------------------------------------------
-- Server Configuration
-- ---------------------------------------------------------------------------

-- | Configuration for the REST API and WebSocket server
data ServerConfig = ServerConfig
  { serverPort :: Int
  -- ^ HTTP port to listen on
  , serverHost :: Text
  -- ^ Host to bind to
  , serverWebSocketPath :: Text
  -- ^ WebSocket endpoint path
  , serverCorsEnabled :: Bool
  -- ^ Whether CORS is enabled
  , serverMaxConnections :: Int
  -- ^ Maximum concurrent WebSocket connections
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Default server: port 8080, localhost, WebSocket at /ws
defaultServerConfig :: ServerConfig
defaultServerConfig =
  ServerConfig
    { serverPort = 8080
    , serverHost = "127.0.0.1"
    , serverWebSocketPath = "/ws"
    , serverCorsEnabled = True
    , serverMaxConnections = 100
    }

-- ---------------------------------------------------------------------------
-- Database Configuration
-- ---------------------------------------------------------------------------

-- | Configuration for the SQLite persistence layer
data DatabaseConfig = DatabaseConfig
  { dbFilePath :: FilePath
  -- ^ Path to SQLite database file
  , dbBusyTimeoutMs :: Int
  -- ^ Busy timeout for concurrent access in milliseconds
  , dbWalMode :: Bool
  -- ^ Whether to enable WAL (Write-Ahead Logging) mode
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Default database: aegis.db in current directory, WAL enabled
defaultDatabaseConfig :: DatabaseConfig
defaultDatabaseConfig =
  DatabaseConfig
    { dbFilePath = "aegis.db"
    , dbBusyTimeoutMs = 5000
    , dbWalMode = True
    }

-- ---------------------------------------------------------------------------
-- Scan Configuration
-- ---------------------------------------------------------------------------

-- | Configuration for a single scan/analysis run
data ScanConfig = ScanConfig
  { scanMaxVulnerabilities :: Int
  -- ^ Maximum number of vulnerabilities to process per run
  , scanMaxRefactorRetries :: Int
  -- ^ Maximum refactor→verify retry cycles before giving up
  , scanRequireHITL :: Bool
  -- ^ Whether human-in-the-loop approval is required
  , scanAutoCommit :: Bool
  -- ^ Whether to automatically commit approved patches
  , scanCreatePR :: Bool
  -- ^ Whether to create pull requests for approved patches
  , scanParallelVulnerabilities :: Int
  -- ^ Number of vulnerabilities to process in parallel
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Default scan: 10 max vulns, 3 retries, HITL required, no auto-commit
defaultScanConfig :: ScanConfig
defaultScanConfig =
  ScanConfig
    { scanMaxVulnerabilities = 10
    , scanMaxRefactorRetries = 3
    , scanRequireHITL = True
    , scanAutoCommit = False
    , scanCreatePR = False
    , scanParallelVulnerabilities = 1
    }

-- ---------------------------------------------------------------------------
-- Top-Level Configuration
-- ---------------------------------------------------------------------------

-- | Top-level AegisCode AI configuration aggregating all sub-configurations
data AegisConfig = AegisConfig
  { configLLM :: LLMProviderConfig
  -- ^ LLM provider settings
  , configTokenBudget :: TokenBudgetConfig
  -- ^ Token budgeting limits
  , configRetry :: RetryConfig
  -- ^ Retry policy
  , configCircuitBreaker :: CircuitBreakerConfig
  -- ^ Circuit breaker settings
  , configRepository :: RepositoryConfig
  -- ^ Target repository settings
  , configSandbox :: SandboxConfig
  -- ^ Sandboxed execution settings
  , configServer :: ServerConfig
  -- ^ REST/WebSocket server settings
  , configDatabase :: DatabaseConfig
  -- ^ SQLite database settings
  , configScan :: ScanConfig
  -- ^ Scan/analysis run settings
  , configLog :: LogConfig
  -- ^ Logging configuration
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Default top-level configuration with all sub-configs set to defaults
defaultAegisConfig :: AegisConfig
defaultAegisConfig =
  AegisConfig
    { configLLM =
        LLMProviderConfig
          { primaryProvider = defaultOllamaConfig
          , fallbackModels = ["llama3.2", "codellama"]
          , maxRetries = 3
          }
    , configTokenBudget = defaultTokenBudgetConfig
    , configRetry = defaultRetryConfig
    , configCircuitBreaker = defaultCircuitBreakerConfig
    , configRepository = defaultRepositoryConfig
    , configSandbox = defaultSandboxConfig
    , configServer = defaultServerConfig
    , configDatabase = defaultDatabaseConfig
    , configScan = defaultScanConfig
    , configLog = defaultLogConfig
    }
