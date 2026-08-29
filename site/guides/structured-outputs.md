---
title: Structured Type Extraction
description: Type-safe JSON Schema extraction into validated Haskell data structures.
category: Guides & Recipes
---

## Type-Safe Structured Output

Instead of parsing unstructured Markdown text from LLMs, `langchain-hs` provides type-safe structured extraction using JSON Schemas and `Aeson` deserializers:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Langchain.Prelude

-- Define desired output structure
data BugReport = BugReport
  { summary    :: Text
  , severity   :: Text -- "low" | "medium" | "critical"
  , components :: [Text]
  , fixSteps   :: [Text]
  } deriving (Show, Generic, FromJSON, ToJSON, StructuredOutput)

main :: IO ()
main = do
  let model = newOllama "qwen2.5:7b" "http://localhost:11434"

  let userLog = "Crash: Null pointer in HTTP connection pool worker thread after 500 requests"

  -- Invokes model with JSON schema constraint and automatically parses result into BugReport
  reportResult <- structuredInvoke @BugReport model userLog
  case reportResult of
    Left err     -> putStrLn ("Parsing/LLM Error: " ++ show err)
    Right report -> do
      putStrLn ("Severity: " ++ show (severity report))
      putStrLn ("Summary: " ++ show (summary report))
```
