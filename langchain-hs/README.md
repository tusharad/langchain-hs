# LangChain Haskell (langchain-hs)

> Type-Safe AI Agents & Multi-Agent Graphs in Haskell

A functional, effect-polymorphic Haskell ecosystem for building production AI applications, featuring pure AST pipelines (`RunnableTree`), cyclic state graphs (`StateGraph`), Model Context Protocol (MCP), and OpenTelemetry observability.

[![Hackage](https://img.shields.io/badge/hackage-0.0.6.0-blue.svg)](https://hackage.haskell.org/package/langchain-hs)
[![GHC](https://img.shields.io/badge/GHC-9.8%2B-purple.svg)](https://www.haskell.org/ghc/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Documentation](https://img.shields.io/badge/docs-website-blueviolet.svg)](https://tusharad.github.io/langchain-hs/)

---

## Installation

Install `langchain-hs` and provider packages via **Cabal**:

```bash
cabal update
cabal install langchain-hs langchain-hs-ollama langchain-hs-openai
```

Or add to your `stack.yaml`:

```yaml
extra-deps:
  - langchain-hs-0.0.6.0
  - langchain-hs-core-0.0.6.0
  - langchain-hs-graph-0.0.6.0
  - langchain-hs-ollama-0.0.6.0
  - langchain-hs-openai-0.0.6.0
```

Package reference on Hackage: [https://hackage.haskell.org/package/langchain-hs](https://hackage.haskell.org/package/langchain-hs)

---

## 1. Simple Chat Calling

Send a prompt to an LLM (local Ollama, OpenAI, or Gemini) and retrieve a typed response:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Except (runExceptT)
import qualified Data.Text.IO as T
import Langchain.Prelude
import Langchain.Provider.Ollama

main :: IO ()
main = do
  -- Connect to local Ollama instance (DeepSeek, Llama 3, Gemma, etc.)
  model <- newOllama "llama3.2" defaultConfig
  
  let messages = [userMessage "Explain in two sentences why Haskell makes AI agents more reliable."]
  result <- runExceptT $ invoke model messages Nothing
  
  case result of
    Left err   -> T.putStrLn $ errorMessage err
    Right resp -> T.putStrLn $ extractMessageText resp
```

### Output

```text
Haskell's static type system prevents common agent failures like missing dictionary keys or unexpected nulls before code ever reaches production. Furthermore, pure state transitions guarantee that autonomous agents cannot silently mutate variables behind your back.
```

---

## 2. JSON Structured Output

Guarantee that the LLM response conforms to a typed Haskell record using `FromJSON` and `ToSchema`:

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Aeson (FromJSON)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import Langchain.Prelude
import Langchain.Provider.Ollama

-- Define your target data schema
data Developer = Developer
  { name              :: T.Text
  , preferredLanguage :: T.Text
  , yearsOfExperience :: Int
  , favoriteLibrary   :: T.Text
  } deriving (Show, Generic, FromJSON, ToSchema)

main :: IO ()
main = do
  model <- newOllama "llama3.2" defaultConfig
  
  let prompt =
        [ userMessage "Extract profile: Alan has been writing Haskell for 8 years and loves langchain-hs." ]
      options = withStructuredOutput (Proxy @Developer) defaultOllamaConfig

  result <- runLangchainT () $ invoke model prompt (Just options)
  case result of
    Left err  -> T.putStrLn $ errorMessage err
    Right msg -> T.putStrLn $ extractMessageText msg
```

### Output

```json
{
  "name": "Alan",
  "preferredLanguage": "Haskell",
  "yearsOfExperience": 8,
  "favoriteLibrary": "langchain-hs"
}
```

---

## 3. Tool Calling

Equip models with effectful Haskell tools. The LLM automatically emits structured tool calls, which your application executes and feeds back for final synthesis:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Langchain.Prelude
import Langchain.Provider.Ollama

main :: IO ()
main = do
  model <- newOllama "qwen2.5:7b" defaultConfig
  
  -- Instantiate typed filesystem tool
  let fsTool   = findFilesTool @IO defaultSecurityPolicy
      toolCfg  = withTools [fsTool] defaultOllamaConfig
      prompt   = [userMessage "Search for Haskell source files using find_files with pattern '*.hs'."]

  res <- runLangchainT () $ do
    response <- invoke model prompt (Just toolCfg)
    case messageToolCalls response of
      Just (tCall:_) -> do
        liftIO $ T.putStrLn $ "Tool Invoked: " <> toolCallName tCall
        liftIO $ T.putStrLn $ "Arguments:    " <> T.pack (show (toolCallArguments tCall))
        
        -- Execute tool with LLM arguments
        execResult <- liftIO $ toolExecute fsTool (toolCallArguments tCall)
        let observation = case execResult of
              Left err  -> "Error: " <> errorMessage err
              Right out -> out
        liftIO $ T.putStrLn $ "Observation:  " <> observation
        
        -- Supply tool observation back to model for final response
        let toolMsg = (toolMessage observation) { messageName = Just (toolCallName tCall) }
            thread  = prompt ++ [response, toolMsg]
            
        finalMsg <- invoke model thread (Just toolCfg)
        liftIO $ T.putStrLn "\nFinal Assistant Answer:"
        liftIO $ T.putStrLn $ extractMessageText finalMsg
      _ -> do
        liftIO $ T.putStrLn $ extractMessageText response

  case res of
    Left err -> T.putStrLn $ "Error: " <> errorMessage err
    Right () -> pure ()
```

### Output

```text
Tool Invoked: find_files
Arguments:    fromList [("pattern",String "*.hs")]
Observation:  ["src/Main.hs","src/Agent.hs","src/Tools.hs"]

Final Assistant Answer:
I scanned the repository and identified 3 Haskell source files matching your pattern:
- src/Main.hs
- src/Agent.hs
- src/Tools.hs
```

---

## 4. ReAct Agent

Run an autonomous **Reason + Act** agent loop that decomposes user tasks, selects tools, evaluates observations, and iterates until completion:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Except (runExceptT)
import qualified Data.Text.IO as T
import Langchain.Prelude
import Langchain.Provider.Ollama

main :: IO ()
main = do
  -- Setup sample file for demonstration
  writeFile "/tmp/server_status.txt" "Service: api-gateway, Status: Degraded, Latency: 420ms"
  
  model <- newOllama "qwen2.5:7b" defaultConfig
  
  -- Equip agent with file inspection tools
  let policy = defaultSecurityPolicy
      tools  = [readFileTool Nothing policy, findFilesTool policy]
      agent  = defaultReActAgent model tools
      prompt = [userMessage "Inspect /tmp/server_status.txt using read_file and summarize the service health."]

  res <- runExceptT $ runReActAgent agent prompt
  case res of
    Left err  -> T.putStrLn $ errorMessage err
    Right msg -> T.putStrLn $ extractMessageText msg
```

### Output

```text
[ReAct Iteration 1]
Thought: I should read the contents of /tmp/server_status.txt to check the current health status.
Action: read_file {"path": "/tmp/server_status.txt"}
Observation: Service: api-gateway, Status: Degraded, Latency: 420ms

[ReAct Iteration 2]
Thought: The file has been read. The api-gateway service is in a degraded state with elevated latency. I will now summarize this for the user.

Final Answer:
The api-gateway service is currently experiencing degraded health with high latency (420ms). Immediate investigation of the gateway service is recommended.
```

---

## Documentation & Resources

- **Hackage Package:** [https://hackage.haskell.org/package/langchain-hs](https://hackage.haskell.org/package/langchain-hs)
- **Documentation Website:** [https://tusharad.github.io/langchain-hs/](https://tusharad.github.io/langchain-hs/)
- **Whitepaper:** [whitepaper.md](whitepaper.md) (Effect polymorphism, categorical design, state graphs)
- **Examples Directory:** [`examples/`](examples/)

---

## Development & Testing

```bash
# Build project
stack build

# Run test suite
stack test

# Code quality checks
make lint
make format-check
```

---

## License

Distributed under the **MIT License**. See [LICENSE](LICENSE) for details.
