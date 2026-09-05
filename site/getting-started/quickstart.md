---
title: Quickstart (5-Minute Guide)
description: Create your first working LLM program in Haskell with Ollama, OpenAI, or Gemini.
category: Getting Started
---

## 1. Minimal Working Example (Ollama)

Make sure you have [Ollama](https://ollama.com/) running locally (`ollama run qwen2.5:7b` or `ollama run llama3.2`):

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except (runExceptT)
import qualified Data.Text.IO as TIO
import Langchain.Prelude

main :: IO ()
main = do
  -- 1. Initialize the ChatModel instance
  model <- newOllama "qwen2.5:7b" defaultConfig

  -- 2. Construct messages
  let messages =
        [ systemMessage "You are a concise, helpful Haskell tutor."
        , userMessage "What is the difference between Functor, Applicative, and Monad?"
        ]

  -- 3. Invoke the model
  res <- runExceptT $ invoke model messages
  case res of
    Left err -> putStrLn ("Invocation Error: " ++ show err)
    Right responseMsg -> do
      putStrLn "--- LLM Response ---"
      TIO.putStrLn (extractMessageText responseMsg)
```

---

## 2. Using OpenAI or Gemini

Switching between model providers requires changing only the model constructor:

```haskell
-- OpenAI
import Langchain.Provider.OpenAI

let model = newOpenAI "gpt-4o" "sk-..."

-- Google Gemini
import Langchain.Provider.Gemini

let model = newGemini "gemini-1.5-pro" "AIza..."
```

---

## 3. Real-Time Streaming Output

`langchain-hs` provides conduit-based reactive event streaming with `StreamEvent`:

```haskell
import Langchain.Prelude

streamExample :: IO ()
streamExample = do
  model <- newOllama "qwen2.5:7b" defaultConfig
  let messages = [userMessage "Count from 1 to 10 with explanations."]

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

<div class="admonition note">
  <div class="admonition-title">📘 What's Next?</div>
  Learn how to equip LLMs with tools and autonomous reasoning in the <a href="/getting-started/first-agent.html">First Agent Guide</a>, or build stateful graph workflows in the <a href="/getting-started/first-graph.html">First StateGraph Guide</a>.
</div>
