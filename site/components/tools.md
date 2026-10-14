---
title: Tools & Security System
description: Type-safe function definitions, sandboxed filesystem tools, process management, checkpoints, and permission tiers.
category: Components
---

<div class="component-header-meta">
  <span class="badge badge-primary">Langchain.Tool</span>
  <span class="badge badge-primary">Langchain.Tool.FileSystem</span>
  <span class="badge badge-primary">Langchain.Tool.Process</span>
  <span class="badge badge-primary">Langchain.Tool.Security</span>
  <span class="badge badge-primary">Langchain.Tool.Checkpoint</span>
</div>

Haskell-native, tools designed specifically for LLM agents. Built with memory and context-window protection, read-before-edit verification, automatic undo checkpoints, asynchronous background process management, and strict tiered security policies.

## Key Capabilities

- **Tiered Permissions**: 4 distinct privilege tiers (`TierReadOnly`, `TierWorkspaceWrite`, `TierExecution`, `TierAdmin`) with configurable policies.
- **Observational File Tools**: Concurrent recursive find (`find_files`) ignoring `.git` / `.stack-work` / `dist-newstyle`, and ripgrep-style content search (`grep_search`) with line numbering and context limits.
- **Context-Aware Reading**: Paginated file reading (`read_file`) with line ranges, line-numbered formatting, and automatic registration into the read-before-edit tracker.
- **Safe Editing & Undo**: String replacement (`edit_file`) requiring prior inspection, ambiguity verification, and atomic snapshot rollbacks (`undo_file_edit`).
- **Managed Long-Lived Processes**: Asynchronous command execution (`command_run`) with timeout auto-backgrounding, log tailing (`command_status`), and process termination (`command_kill`).
- **Human-in-the-Loop (HITL)**: First-class approval hooks before executing high-risk operations.

## Working Code Example

Compare tool invocation across **Ollama**, **OpenAI / OpenRouter**, and **Google Gemini**:

::: {.provider-group}
::: {.provider-panel data-provider="ollama" data-label="🦙 Ollama (Local)" data-exe="stack run toolollama"}
```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Ollama.Tool (runApp) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Langchain.Prelude
import Langchain.Provider.Ollama
  ( defaultConfig
  , defaultOllamaConfig
  , newOllama
  , withTools
  )

inputPrompt :: T.Text
inputPrompt = "Search for Haskell files using find_files with pattern '*.hs'."

runApp :: IO ()
runApp = do
  o <- newOllama "qwen3.5:2b" defaultConfig
  let promptMsgs = [userMessage inputPrompt]
      fsTool = findFilesTool @IO defaultSecurityPolicy
      toolCfg = withTools [fsTool] defaultOllamaConfig

  res <- runLangchainT () $ do
    respMsg <- invoke o promptMsgs (Just toolCfg)
    case messageToolCalls respMsg of
      Nothing -> liftIO $ T.putStrLn $ extractMessageText respMsg
      Just [] -> liftIO $ T.putStrLn $ extractMessageText respMsg
      Just (tCall : _) -> do
        liftIO $ T.putStrLn $ "Tool called: " <> toolCallName tCall
        liftIO $ T.putStrLn $ "Arguments: " <> T.pack (show (toolCallArguments tCall))

        eExec <- liftIO $ toolExecute fsTool (toolCallArguments tCall)
        toolResult <- case eExec of
          Left err -> pure $ "Tool execution error: " <> errorMessage err
          Right out -> pure out

        liftIO $ T.putStrLn $ "Tool execution result:\n" <> toolResult

        let toolMsg = (toolMessage toolResult) { messageName = Just (toolCallName tCall) }
            conversation = promptMsgs ++ [respMsg, toolMsg]

        finalMsg <- invoke o conversation (Just toolCfg)
        liftIO $ T.putStrLn "\nFinal Assistant Answer:"
        liftIO $ T.putStrLn $ extractMessageText finalMsg

  case res of
    Left err -> T.putStrLn $ "Error: " <> errorMessage err
    Right () -> pure ()
```
:::

::: {.provider-panel data-provider="openai" data-label="⚡ OpenAI / OpenRouter" data-exe="stack run toolopenai"}
```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module OpenAI.Tool (runApp) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Langchain.Prelude
import Langchain.Provider.OpenAI (OpenAI)
import OpenAI.Common (defaultModelName, getOpenRouterModel)

inputPrompt :: T.Text
inputPrompt = "Please search for Haskell source files using find_files with pattern '*.hs'."

runApp :: IO ()
runApp = do
  o <- getOpenRouterModel defaultModelName
  let promptMsgs = [userMessage inputPrompt]
      fsTool = findFilesTool @IO defaultSecurityPolicy
      tools = [fsTool]
      chatReq = bindToolsConfig @OpenAI tools Nothing

  res <- runLangchainT () $ do
    respMsg <- invoke o promptMsgs chatReq
    case messageToolCalls respMsg of
      Nothing -> liftIO $ T.putStrLn $ extractMessageText respMsg
      Just [] -> liftIO $ T.putStrLn $ extractMessageText respMsg
      Just (tCall : _) -> do
        liftIO $ T.putStrLn $ "Tool called: " <> toolCallName tCall
        liftIO $ T.putStrLn $ "Arguments: " <> T.pack (show (toolCallArguments tCall))

        eExec <- liftIO $ toolExecute fsTool (toolCallArguments tCall)
        toolResult <- case eExec of
          Left err -> pure $ "Tool execution error: " <> errorMessage err
          Right out -> pure out

        liftIO $ T.putStrLn $ "Tool execution result:\n" <> toolResult

        let toolMsg =
              (toolMessage toolResult)
                { messageName = Just (toolCallName tCall)
                , messageToolId = Just (toolCallId tCall)
                }
            conversation = promptMsgs ++ [respMsg, toolMsg]
            followUpReq = bindToolsConfig @OpenAI tools Nothing

        finalMsg <- invoke o conversation followUpReq
        liftIO $ T.putStrLn "\nFinal Assistant Answer:"
        liftIO $ T.putStrLn $ extractMessageText finalMsg

  case res of
    Left err -> T.putStrLn $ "Error: " <> errorMessage err
    Right () -> pure ()
```
:::

::: {.provider-panel data-provider="gemini" data-label="✨ Google Gemini" data-exe="cabal run toolgemini"}
```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Gemini.Tool (runApp) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Gemini.Common (defaultGeminiModelName, getGeminiModel)
import Langchain.Prelude
import Langchain.Provider.Gemini (Gemini)

inputPrompt :: T.Text
inputPrompt = "Search for Haskell files using find_files with pattern '*.hs'."

runApp :: IO ()
runApp = do
  T.putStrLn "--- Gemini Tool / Function Calling Loop ---"
  g <- getGeminiModel defaultGeminiModelName
  let promptMsgs = [userMessage inputPrompt]
      fsTool = findFilesTool @IO defaultSecurityPolicy
      tools = [fsTool]
      chatReq = bindToolsConfig @Gemini tools Nothing

  res <- runLangchainT () $ do
    respMsg <- invoke g promptMsgs chatReq
    case messageToolCalls respMsg of
      Nothing -> liftIO $ T.putStrLn $ extractMessageText respMsg
      Just [] -> liftIO $ T.putStrLn $ extractMessageText respMsg
      Just (tCall : _) -> do
        liftIO $ T.putStrLn $ "Tool called: " <> toolCallName tCall
        liftIO $ T.putStrLn $ "Arguments: " <> T.pack (show (toolCallArguments tCall))

        eExec <- liftIO $ toolExecute fsTool (toolCallArguments tCall)
        toolResult <- case eExec of
          Left err -> pure $ "Tool execution error: " <> errorMessage err
          Right out -> pure out

        liftIO $ T.putStrLn $ "Tool execution result:\n" <> toolResult

        let toolMsg =
              (toolMessage toolResult)
                { messageName = Just (toolCallName tCall)
                , messageToolId = Just (toolCallId tCall)
                }
            conversation = promptMsgs ++ [respMsg, toolMsg]
            followUpReq = bindToolsConfig @Gemini tools Nothing

        finalMsg <- invoke g conversation followUpReq
        liftIO $ T.putStrLn "\nFinal Assistant Answer:"
        liftIO $ T.putStrLn $ extractMessageText finalMsg

  case res of
    Left err -> T.putStrLn $ "Error: " <> errorMessage err
    Right () -> pure ()
```
:::
:::

## Complete Tool Suite Reference

### 1. Filesystem Suite (`Langchain.Tool.FileSystem`)

| Tool Name | Tier | Parameters | Description |
|-----------|------|------------|-------------|
| `find_files` | `TierReadOnly` | `path`, `pattern`, `max_depth`, `file_type`, `max_results` | Concurrent search ignoring `.git`, `node_modules`, `dist-newstyle`, etc. |
| `grep_search` | `TierReadOnly` | `path`, `query`, `case_sensitive`, `include_glob`, `context_lines`, `max_results` | Ripgrep-style search with line numbers, context slices, and truncation limits. |
| `read_file` | `TierReadOnly` | `path`, `start_line`, `end_line` | Paginated reading with 1-indexed line numbers and read-before-edit tracking. |
| `list_directory` | `TierReadOnly` | `path` | Lists directories and files with formatted sizes (e.g. `12 KB`). |
| `edit_file` | `TierWorkspaceWrite` | `path`, `old_string`, `new_string`, `expected_occurrences` | Enforces read-before-edit policy, verifies uniqueness, creates undo checkpoint. |
| `write_file` | `TierWorkspaceWrite` | `path`, `content`, `overwrite` | Creates file and parent directories. Requires `overwrite=true` to overwrite. |

### 2. Checkpoints & Rollback (`Langchain.Tool.Checkpoint`)

| Tool Name | Tier | Parameters | Description |
|-----------|------|------------|-------------|
| `undo_file_edit` | `TierWorkspaceWrite` | `path`, `checkpoint_id` (optional) | Reverts a file to its previous state before edit or overwrite. |
| `list_checkpoints` | `TierReadOnly` | `path` (optional) | Inspect audit trail of snapshots taken during the session. |

### 3. Managed Background Processes (`Langchain.Tool.Process`)

| Tool Name | Tier | Parameters | Description |
|-----------|------|------------|-------------|
| `command_run` | `TierExecution` | `command`, `cwd`, `timeout_ms` | Runs command with synchronous timeout (default: 5000ms). If exceeded, automatically backgrounds to `proc-<id>`. |
| `command_status` | `TierReadOnly` | `process_id`, `tail_lines` | Returns runtime duration, exit status, and tails stdout/stderr logs. |
| `command_kill` | `TierExecution` | `process_id` | Terminate running background process and cleans up resources. |

## Security & Permission Tiers (`Langchain.Tool.Security`)

Every tool is bound to a `ToolTier`:

```haskell
data ToolTier
  = TierReadOnly       -- Safe queries: read, find, grep, list, status
  | TierWorkspaceWrite  -- File modifications within workspace
  | TierExecution       -- Process launch, shell commands
  | TierAdmin           -- Unrestricted system changes
```

### Configurable Security Policies

```haskell
-- Restrict agent strictly to workspace directory
let policy = workspaceSandboxPolicy "/home/user/project"

-- Restrict agent strictly to read-only observation
let readOnlyPolicy = readOnlySecurityPolicy

-- Attach an interactive Human-in-the-Loop approval hook
let approvalPolicy = defaultSecurityPolicy
      { policyApprovalHook = Just $ \req -> do
          putStrLn $ "Approve " ++ show (approvalToolName req) ++ "? [y/n]"
          ans <- getLine
          pure $ if ans == "y" then Approved else Denied "User denied execution"
      }
```
