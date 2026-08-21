---
title: Building Your First Agent
description: Learn how to construct ReAct and Plan-and-Execute autonomous agents with custom typed tools.
category: Getting Started
---

## What is an Agent?

Unlike a basic linear prompt-response cycle, an **Agent** uses the model to dynamically choose which tools to invoke, interpret their outputs (observations), and iterate until it reaches a conclusive answer.

```
       ┌─────────────┐
       │   User Q    │
       └──────┬──────┘
              ▼
   ┌──────────────────────┐
┌─►│   LLM Reasoning      ├─┐ (Final Answer)
│  └──────────┬───────────┘ │
│ (Observation)│ (Tool Call) │
│             ▼             │
│  ┌──────────────────────┐ │
└──┤   Tool Execution     │ │
   └──────────────────────┘ │
              ▼             ▼
       ┌──────────────────────┐
       │     Final Answer     │
       └──────────────────────┘
```

---

## 1. Creating Typed Tools

In `langchain-hs`, tools are first-class `Tool m` records with JSON Schema parameter definitions and typed execution handlers:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (Value(..), object, (.=))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Langchain.Prelude

-- Define a custom calculator tool
calculatorTool :: Tool IO
calculatorTool = Tool
  { toolName = "calculator"
  , toolDescription = "Evaluates basic arithmetic expressions. Input should be e.g. '12 * 45'."
  , toolParameters = object
      [ "type" .= ("object" :: T.Text)
      , "properties" .= object
          [ "expression" .= object
              [ "type" .= ("string" :: T.Text)
              , "description" .= ("Arithmetic expression" :: T.Text)
              ]
          ]
      , "required" .= (["expression"] :: [T.Text])
      ]
  , toolExecute = \args -> do
      case Map.lookup "expression" args of
        Just "12 * 45" -> pure "540"
        Just expr      -> pure $ "Evaluated: " <> expr
        Nothing        -> pure "Error: Missing expression argument"
  }
```

---

## 2. Running a ReAct Agent

Construct a `ReActAgent` with your LLM, toolset, and iteration limits:

```haskell
main :: IO ()
main = do
  let model = newOllama "qwen2.5:7b" "http://localhost:11434"
  let tools = [calculatorTool]
  
  -- Create ReAct Agent with default limits (max 15 iterations)
  let agent = createReActAgent model tools defaultAgentConfig

  putStrLn "Running agent..."
  res <- runReActAgent agent "What is 12 * 45 plus 10?"
  case res of
    Left err  -> putStrLn ("Agent Error: " ++ show err)
    Right ans -> putStrLn ("Final Answer:\n" ++ T.unpack ans)
```

---

## 3. Plan-and-Execute Agent

For complex multi-step problems, a `PlanAndExecuteAgent` uses two specialized models (or prompts):
1. **Planner**: Creates an explicit decomposition plan.
2. **Executor**: Executes each step sequentially using available tools and updates the plan.

```haskell
planAndExecuteExample :: IO ()
planAndExecuteExample = do
  let model = newOllama "qwen2.5:7b" "http://localhost:11434"
  let agent = newPlanAndExecuteAgent model model (Just [calculatorTool])

  res <- runExceptT $ runPlanAndExecute agent "Calculate (12 * 45) + 10 and format the explanation."
  case res of
    Left err -> putStrLn ("Execution failed: " ++ show err)
    Right response -> print response
```

<div class="admonition tip">
  <div class="admonition-title">💡 Middleware Support</div>
  You can attach logging, rate-limiting, and guardrail middleware to agents with <code>chainMiddlewares [loggingMiddleware, defaultMiddleware]</code>.
</div>
