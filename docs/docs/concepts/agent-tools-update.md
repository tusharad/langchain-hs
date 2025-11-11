# Agent Tool Integration (Updated)

## Overview

The Agent module now uses the Tool typeclass directly with a simple wrapper function, eliminating the need for a separate Tools module.

## Tool Wrapping

### The `AnyTool` Type

Tools are wrapped using the `AnyTool` existential type:

```haskell
data AnyTool = forall t. Tool t => AnyTool
  { anyToolInstance :: t
  , anyToolInputParser :: Text -> Maybe (Input t)
  , anyToolOutputFormatter :: Output t -> Text
  }
```

### Wrapping Tools

Use the `wrapTool` function to convert any `Tool` instance:

```haskell
import Langchain.Agent.Core (wrapTool)
import Langchain.Tool.Calculator

-- Wrap a calculator tool
let calculatorTool = wrapTool
      CalculatorTool
      (Just . id)                    -- Input parser
      (either id (T.pack . show))    -- Output formatter
```

## Creating Agents

Agents now take a list of `AnyTool`:

```haskell
import Langchain.Agent.Core
import Langchain.Agent.ReAct
import Langchain.Tool.Calculator
import Langchain.Tool.WebScraper

main :: IO ()
main = do
  -- Wrap your tools
  let calculator = wrapTool CalculatorTool (Just . id) (either id (T.pack . show))
      webScraper = wrapTool WebScraper (Just . id) (either id id)
  
  -- Create agent with list of tools
  let llm = Ollama "llama3.2" []
      agent = createReActAgent llm [calculator, webScraper]
  
  -- Run agent
  result <- runAgentExecutor agent defaultAgentConfig defaultAgentCallbacks
    "What is 25 * 4?"
```

## Benefits

1. **Simpler**: No separate registry module needed
2. **Type-safe**: Uses the existing Tool typeclass
3. **Flexible**: Custom parsers/formatters per tool
4. **Direct**: Less indirection, easier to understand

## Migration from Old API

If you were using the old `ToolRegistry`:

**Before:**
```haskell
let tools = createToolRegistry [tool1, tool2]
    agent = createReActAgent llm tools
```

**After:**
```haskell
let tools = [wrapTool tool1 parser1 formatter1, wrapTool tool2 parser2 formatter2]
    agent = createReActAgent llm tools
```

## Complete Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Langchain.Agent.Core
import Langchain.Agent.ReAct
import Langchain.Agent.Executor
import Langchain.Tool.Calculator
import Langchain.LLM.Ollama
import qualified Data.Text as T

main :: IO ()
main = do
  -- Create LLM
  let llm = Ollama "llama3.2" []
  
  -- Wrap tools with custom parsers/formatters
  let calculator = wrapTool
        CalculatorTool
        (Just . id)                    -- Parse as-is
        (either id (T.pack . show))    -- Format output
  
  -- Create agent with tools
  let agent = createReActAgent llm [calculator]
  
  -- Configure execution
  let config = defaultAgentConfig { maxIterations = 10 }
  
  -- Run
  result <- runAgentExecutor agent config defaultAgentCallbacks
    "Calculate (15 + 25) * 2"
  
  case result of
    Left err -> putStrLn $ "Error: " <> show err
    Right execResult ->
      putStrLn $ T.unpack (agentOutput $ executionFinish execResult)
```

