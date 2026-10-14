---
title: Quickstart (5-Minute Guide)
description: Create your first working LLM program in Haskell with Ollama, OpenAI, or Gemini.
category: Getting Started
---

## 1. Minimal Working Example (Ollama)

Make sure you have [Ollama](https://ollama.com/) running locally (`ollama run llama3.2` or `ollama run qwen2.5:7b`):

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Except (runExceptT)
import qualified Data.Text.IO as TIO
import Langchain.Prelude
import Langchain.Provider.Ollama

main :: IO ()
main = do
  -- 1. Initialize the ChatModel instance
  model <- newOllama "llama3.2" defaultConfig

  -- 2. Construct messages
  let messages =
        [ systemMessage "You are a concise, helpful Haskell tutor."
        , userMessage "What is the difference between Functor, Applicative, and Monad in one sentence?"
        ]

  -- 3. Invoke the model
  res <- runExceptT $ invoke model messages Nothing
  case res of
    Left err -> putStrLn ("Invocation Error: " ++ show err)
    Right responseMsg -> do
      putStrLn "--- LLM Response ---"
      TIO.putStrLn (extractMessageText responseMsg)
```

### Execution Output

```text
--- LLM Response ---
A Functor maps a normal function over wrapped values, an Applicative maps a wrapped function over wrapped values, and a Monad feeds a wrapped value into a function that returns a wrapped value.
```

---

## 2. Using OpenAI or Gemini

Switching between model providers requires changing only the model constructor:

```haskell
-- OpenAI
import Langchain.Provider.OpenAI

let model = newOpenAI "sk-..." "gpt-4o"

-- Google Gemini (Interactions v1 API)
import Langchain.Provider.Gemini

let model = newGemini "AIzaSy..." "gemini-3.6-flash" Nothing
```

---

## 3. Real-Time Streaming Output

`langchain-hs` provides conduit-based reactive event streaming with `StreamEvent`:

```haskell
import Langchain.Prelude
import Langchain.Provider.Ollama

streamExample :: IO ()
streamExample = do
  model <- newOllama "llama3.2" defaultConfig
  let messages = [userMessage "Explain Monads briefly."]

  -- Stream tokens directly to stdout as they arrive
  streamModel model messages $ \event ->
    case event of
      LLMStart -> putStrLn "[Stream Started]"
      LLMChunk chunkText -> putStr (show chunkText)
      LLMEnd usage -> putStrLn "\n[Stream Finished]"
      _ -> pure ()
```

---

## 4. Pure AST Pipelines (`RunnableTree`)

Instead of evaluating LLM calls eagerly, build a declarative AST and interpret it:

```haskell
pipeline :: RunnableTree IO Text Text
pipeline =
      runLambda (\q -> "Answer in 1 sentence: " <> q)
  |>> invokeLLM model
  |>> runLambda extractMessageText

runIt :: IO ()
runIt = do
  answer <- interpret pipeline "Why is Haskell great for AI?"
  putStrLn (show answer)
```

<div class="admonition">
  <div class="admonition-title">What's Next?</div>
  Learn how to equip LLMs with tools and autonomous reasoning in the <a href="/getting-started/first-agent.html">First Agent Guide</a>, or build stateful graph workflows in the <a href="/getting-started/first-graph.html">First StateGraph Guide</a>. You can also view package releases on <a href="https://hackage.haskell.org/package/langchain-hs" target="_blank" rel="noopener noreferrer">Hackage</a>.
</div>
