# Getting Started with LangChain-HS

`langchain-hs` is the modern, pure, effect-polymorphic Haskell ecosystem for building LLM applications, agentic workflows, multi-agent graphs, RAG pipelines, and Model Context Protocol (MCP) integrations.

---

## 1. Quick Installation

Add `langchain-hs`, `langchain-hs-core`, and `langchain-hs-graph` to your `package.yaml` or `.cabal` file:

```yaml
dependencies:
  - langchain-hs
  - langchain-hs-core
  - langchain-hs-graph
  - text
  - aeson
```

---

## 2. Hello, World: Calling an LLM

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Except (runExceptT)
import Langchain.Prelude

main :: IO ()
main = do
  -- Connect to local Ollama instance running qwen2.5:7b
  let model = newOllama "qwen2.5:7b" "http://localhost:11434"
  
  result <- runExceptT $ invoke model [userMessage "Explain Monads in one concise sentence."] Nothing
  case result of
    Left err  -> putStrLn ("Error: " ++ show err)
    Right msg -> putStrLn ("LLM Answer:\n" ++ show (extractMessageText msg))
```

---

## 3. Streaming Responses

```haskell
import Data.Conduit
import qualified Data.Conduit.List as CL
import Langchain.Prelude

streamExample :: IO ()
streamExample = do
  let model = newOllama "qwen2.5:7b" "http://localhost:11434"
  runExceptT $ runConduit $
    stream model [userMessage "Count to 5."] Nothing
      .| CL.mapM_ printEvents
  pure ()
```

---

## 4. Pure AST Pipelines (`RunnableTree`)

Compose pure, inspectable computation trees with algebraic identity and associativity laws:

```haskell
let pipeline = 
      runLambda (\input -> pure $ Right ("Prompt: " <> input))
      |>> runLambda (\prompt -> pure $ Right (prompt <> " -> Processed"))
```
