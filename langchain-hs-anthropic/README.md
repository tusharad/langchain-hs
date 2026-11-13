# langchain-hs-anthropic

Native Anthropic Claude Messages API (`/v1/messages`) provider for [langchain-hs](https://hackage.haskell.org/package/langchain-hs).

Provides `Langchain.Provider.Anthropic` implementing `ChatModel`, `ToolBinder`, and `CacheableChatModel` without pulling in OpenAI, Gemini, or Ollama dependencies.

## Features

- Native `/v1/messages` client with top-level `system` instructions, multi-modal `text` and `image` blocks, `tool_use` / `tool_result` mapping, prompt caching (`cache_control`), and thinking budgets.
- Real-time SSE streaming (`message_start`, `content_block_start`, `content_block_delta` with `text_delta` & `input_json_delta`, `content_block_stop`, `message_delta`, `message_stop`) emitting `LLMStart`, `LLMChunk`, and `LLMEnd` with `TokenUsage`.
- Automatic `x-api-key` redaction in all error payloads.

## Installation

```yaml
# stack.yaml extra-deps
- langchain-hs-anthropic-0.0.6.2
```

```yaml
# package.yaml / .cabal
dependencies:
  - langchain-hs-anthropic >= 0.0.6.2 && < 0.0.7
  - langchain-hs-core >= 0.0.6.2 && < 0.0.7
```

## Usage

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Except (runExceptT)
import Langchain.Model
import Langchain.Provider.Anthropic

main :: IO ()
main = do
  let claude = newAnthropic "sk-ant-..." "claude-sonnet-4-5"
  res <- runExceptT $ invoke claude [userMessage "Explain algebraic effects in 2 sentences."] Nothing
  print res
```

