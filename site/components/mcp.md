---
title: Model Context Protocol (MCP)
description: Standardized tool and resource integration over stdio and HTTP JSON-RPC.
category: Components
---

<div class="component-header-meta">
  <span class="badge badge-primary">Langchain.MCP.Client</span>
</div>

Standardized tool and resource integration over stdio and HTTP JSON-RPC.

## Key Concepts

- **Open Standard**: Connect to external tools, databases, and filesystem servers conforming to Anthropic's Model Context Protocol.
- **Process Isolation**: Run tool servers in separate processes communicating securely over standard input/output.
- **Dynamic Tool Discovery**: Automatically inspect server capabilities and convert MCP tools into Haskell `Tool` instances.

## Working Code Example

Compare local execution via **Ollama** and cloud API execution via **OpenAI / OpenRouter**. Use the toggle tabs or the global provider switcher in the header to switch:

::: {.provider-group}
::: {.provider-panel data-provider="ollama" data-label="🦙 Ollama (Local)" data-exe="stack run mcpollama"}
```haskell
{-# LANGUAGE OverloadedStrings #-}

module Ollama.MCP (runApp) where

import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Langchain.Prelude

runApp :: IO ()
runApp = do
  let client_ =
        newStdioMcpClient
          "hackage-doc"
          "docker"
          ["run", "-i", "--rm", "tusharknight8/hackage-doc-mcp:latest"]

  res <- runLangchainT () $ do
    mcpTools <- listMcpTools client_
    let lcTools = map (mcpToolToLangchainTool client_) mcpTools
    liftIO $
      mapM_
        (\t -> T.putStrLn $ "Tool: " <> mcpToolName t <> " - " <> mcpToolDescription t)
        mcpTools

    o <- newOllama "qwen3.5:2b" defaultConfig
    let msgs = [userMessage "Search Hoogle for the Haskell function 'traverse' using the search tool."]
        req = withTools lcTools (chatRequestFor o msgs)

    resp <- invoke o msgs (Just req)
    case messageToolCalls resp of
      Just (tc : _) -> do
        liftIO $ T.putStrLn $ "\nLLM selected tool: " <> toolCallName tc
        liftIO $ T.putStrLn $ "Arguments: " <> T.pack (show (toolCallArguments tc))

        case find (\t -> toolName t == toolCallName tc) lcTools of
          Just tool -> do
            eOut <- liftIO $ toolExecute tool (toolCallArguments tc)
            let toolResult = case eOut of
                  Left err -> "Error: " <> errorMessage err
                  Right out -> out
                toolMsg = (toolMessage toolResult) {messageName = Just (toolCallName tc)}
                conv = msgs ++ [resp, toolMsg]
                followReq = withTools lcTools (chatRequestFor o conv)

            finalResp <- invoke o conv (Just followReq)
            liftIO $ T.putStrLn "\nAI:"
            liftIO $ T.putStrLn $ extractMessageText finalResp
          Nothing ->
            liftIO $ T.putStrLn "Tool not found."
      _ -> do
        liftIO $ T.putStrLn "No tool called by LLM."
        liftIO $ T.putStrLn $ extractMessageText resp

  case res of
    Left err -> T.putStrLn $ "Error: " <> errorMessage err
    Right () -> pure ()
```
:::

::: {.provider-panel data-provider="openai" data-label="⚡ OpenAI / OpenRouter" data-exe="stack run mcpopenai"}
```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module OpenAI.MCP (runApp) where

import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Langchain.Prelude
import OpenAI.Common (defaultModelName, getOpenRouterModel)

runApp :: IO ()
runApp = do
  let client_ =
        newStdioMcpClient
          "hackage-doc"
          "docker"
          ["run", "-i", "--rm", "tusharknight8/hackage-doc-mcp:latest"]

  res <- runLangchainT () $ do
    mcpTools <- listMcpTools client_
    let lcTools = map (mcpToolToLangchainTool client_) mcpTools
    liftIO $
      mapM_
        (\t -> T.putStrLn $ "Tool: " <> mcpToolName t <> " - " <> mcpToolDescription t)
        mcpTools

    o <- liftIO $ getOpenRouterModel defaultModelName
    let msgs = [userMessage "Search Hoogle for the Haskell function 'traverse' using the search tool."]
        req = bindToolsConfig @OpenAI lcTools Nothing

    resp <- invoke o msgs req
    case messageToolCalls resp of
      Just (tc : _) -> do
        liftIO $ T.putStrLn $ "\nLLM selected tool: " <> toolCallName tc
        liftIO $ T.putStrLn $ "Arguments: " <> T.pack (show (toolCallArguments tc))

        case find (\t -> toolName t == toolCallName tc) lcTools of
          Just tool -> do
            eOut <- liftIO $ toolExecute tool (toolCallArguments tc)
            let toolResult = case eOut of
                  Left err -> "Error: " <> errorMessage err
                  Right out -> out
                toolMsg =
                  (toolMessage toolResult)
                    { messageName = Just (toolCallName tc)
                    , messageToolId = Just (toolCallId tc)
                    }
                conv = msgs ++ [resp, toolMsg]
                followReq = bindToolsConfig @OpenAI lcTools Nothing

            finalResp <- invoke o conv followReq
            liftIO $ T.putStrLn "\nAI:"
            liftIO $ T.putStrLn $ extractMessageText finalResp
          Nothing ->
            liftIO $ T.putStrLn "Tool not found."
      _ -> do
        liftIO $ T.putStrLn "No tool called by LLM."
        liftIO $ T.putStrLn $ extractMessageText resp

  case res of
    Left err -> T.putStrLn $ "Error: " <> errorMessage err
    Right () -> pure ()
```
:::
:::

## Core Types & Functions

```haskell
McpConfig -> (McpClient -> IO a) -> IO a
```
```haskell
McpClient -> IO [Tool]
```
```haskell
McpClient -> Text -> Value -> IO Value
```

## Running This Example

### Local Ollama
Ensure your Ollama daemon is running locally with the target model:
```bash
ollama run gemma3 # or your desired model
stack run mcpollama
```

### OpenAI / OpenRouter
Ensure your `OPENROUTER_API_KEY` or `OPENAI_API_KEY` is exported:
```bash
export OPENROUTER_API_KEY="your-api-key"
stack run mcpopenai
```
