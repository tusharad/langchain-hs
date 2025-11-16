# Agents

Agents are systems that use Large Language Models (LLMs) to determine which actions to take and in what order. An agent combines reasoning with tool execution to accomplish complex tasks autonomously.

## Overview

The agent system in `langchain-hs` follows a **plan-execute-observe** loop:

1. **Plan**: The LLM decides what action to take (which tool to call) or when to finish
2. **Execute**: The selected tool is executed with the provided arguments
3. **Observe**: The result is fed back to the LLM for the next iteration

This continues until the agent reaches a stopping condition (completes the task or hits iteration limits).

## Core Concepts

### Agent Typeclass

The `Agent` typeclass defines the interface that all agents must implement:

```haskell
class Agent a where
  -- Plan the next action or decide to finish
  plan :: a -> AgentState -> IO (LangchainResult (Either AgentAction AgentFinish))
  
  -- Get available tools
  getTools :: a -> [ToolAcceptingToolCall]
  
  -- Execute a tool call
  executeTool :: a -> ToolCall -> IO (LangchainResult Text)
  
  -- Initialize agent state (optional, has default)
  initialize :: a -> AgentState -> IO (LangchainResult AgentState)
  
  -- Cleanup after execution (optional, has default)
  finalize :: a -> AgentState -> IO ()
```

### Agent State

The `AgentState` tracks the agent's execution context:

```haskell
data AgentState = AgentState
  { agentChatHistory :: ChatHistory    -- Message history with the LLM
  , agentInput :: Text                  -- Current user query
  , agentIterations :: Int              -- Number of iterations so far
  }
```

### Agent Actions and Results

**AgentAction** - Represents a decision to call one or more tools:

```haskell
data AgentAction = AgentAction
  { actionToolCall :: [ToolCall]        -- Tool calls to execute
  , actionLog :: Text                   -- LLM's reasoning
  , actionMetadata :: Map Text Text     -- Additional metadata
  }
```

**AgentFinish** - Represents task completion:

```haskell
data AgentFinish = AgentFinish
  { agentOutput :: Text                 -- Final answer
  , finishMetadata :: Map Text Text     -- Execution metadata
  , finishLog :: Text                   -- Final thoughts
  }
```

**AgentStep** - One complete iteration:

```haskell
data AgentStep = AgentStep
  { stepAction :: AgentAction           -- Action taken
  , stepObservation :: Text             -- Tool result
  , stepTimestamp :: UTCTime            -- When it occurred
  }
```

### Configuration

**AgentConfig** - Controls execution limits:

```haskell
data AgentConfig = AgentConfig
  { maxIterations :: Int                -- Maximum steps (default: 15)
  , maxExecutionTime :: Maybe Int       -- Timeout in seconds
  , verboseLogging :: Bool              -- Enable logging (default: False)
  }

defaultAgentConfig :: AgentConfig
```

**AgentCallbacks** - Hooks into the agent lifecycle:

```haskell
data AgentCallbacks = AgentCallbacks
  { onAgentStart :: Text -> IO ()
  , onAgentAction :: AgentAction -> IO ()
  , onAgentObservation :: Text -> IO ()
  , onAgentFinish :: AgentFinish -> IO ()
  , onAgentError :: LangchainError -> IO ()
  , onAgentStep :: AgentStep -> IO ()
  }

defaultAgentCallbacks :: AgentCallbacks
```

## ReAct Agent

ReAct (Reasoning + Acting) is an agent pattern that interleaves reasoning traces with task-specific actions. The LLM alternates between "thinking" about what to do and "acting" by calling tools.

### Creating a ReAct Agent

```haskell
createReActAgent ::
  llm ->                           -- The language model
  Maybe (LLMParams llm) ->         -- LLM parameters
  [ToolAcceptingToolCall] ->       -- Available tools
  ReActAgent llm

createReActAgentWithPrompt ::
  llm ->                           -- The language model
  Maybe (LLMParams llm) ->         -- LLM parameters
  [ToolAcceptingToolCall] ->       -- Available tools
  Text ->                          -- Custom system prompt
  ReActAgent llm
```

### System Prompt

The default ReAct system prompt is:

```haskell
reActSystemPrompt :: Text
reActSystemPrompt = 
  "You are a helpful AI assistant that uses tools to answer user questions."
```

You can customize this using `createReActAgentWithPrompt`.

## Tools for Agents

### ToolAcceptingToolCall

**IMPORTANT**: Agents work with a special type of tool called `ToolAcceptingToolCall`. This is a wrapper around tools that:

- Accept `ToolCall` as input
- Return `Text` as output

```haskell
data ToolAcceptingToolCall where
  ToolAcceptingToolCall ::
    ( Tool t
    , Input t ~ ToolCall
    , Output t ~ Text
    ) => t -> ToolAcceptingToolCall
```

### Creating Agent-Compatible Tools

You must create tools specifically for agent use. Here's the pattern:

```haskell
import Data.Aeson (Value(..))
import qualified Data.Map as HM
import Langchain.Tool.Core
import Langchain.LLM.Core (ToolCall(..), ToolFunction(..))

-- 1. Define your tool type
data MyTool = MyTool

-- 2. Implement Tool with ToolCall input and Text output
instance Tool MyTool where
  type Input MyTool = ToolCall
  type Output MyTool = Text
  
  toolName _ = "my_tool"
  
  toolDescription _ = "Description of what this tool does"
  
  runTool _ (ToolCall _ _ ToolFunction{..}) = do
    -- Check tool name matches
    if toolFunctionName == "my_tool"
      then do
        -- Extract arguments from toolFunctionArguments (Map Text Value)
        case HM.lookup "param_name" toolFunctionArguments of
          Nothing -> pure "Error: missing parameter"
          Just (String value) -> do
            -- Your tool logic here
            pure $ "Result: " <> value
          _ -> pure "Error: invalid parameter type"
      else pure "Error: tool name mismatch"
```

### Example: Age Finder Tool

Here's a complete example from the codebase:

```haskell
data AgeFinderTool = AgeFinderTool

instance Tool AgeFinderTool where
  type Input AgeFinderTool = ToolCall
  type Output AgeFinderTool = Text
  
  toolName _ = "age_finder"
  
  toolDescription _ = "Finds the age of a person given their name."
  
  runTool _ (ToolCall _ _ ToolFunction{..}) = do
    if toolFunctionName == "age_finder"
      then do
        case HM.lookup "name" toolFunctionArguments of
          Nothing -> pure "Unknown"
          Just (String name_) -> pure $ getAge name_
          _ -> pure "Unknown"
      else pure "Unknown"
    where
      getAge name = case name of
        "Alice" -> "40"
        "Bob" -> "45"
        _ -> "Unknown"
```

### Wrapping Existing Tools

If you have existing tools with different input/output types, you need to create a **new wrapper tool** that:

1. Accepts `ToolCall` as input
2. Extracts arguments from the `ToolCall`
3. Calls your existing tool
4. Returns the result as `Text`

```haskell
-- Your existing tool
data MyExistingTool = MyExistingTool

instance Tool MyExistingTool where
  type Input MyExistingTool = MyCustomInput
  type Output MyExistingTool = MyCustomOutput
  -- ... implementation

-- Create a wrapper for agent use
data MyExistingToolWrapper = MyExistingToolWrapper MyExistingTool

instance Tool MyExistingToolWrapper where
  type Input MyExistingToolWrapper = ToolCall
  type Output MyExistingToolWrapper = Text
  
  toolName _ = "my_existing_tool"
  toolDescription _ = "..."
  
  runTool (MyExistingToolWrapper originalTool) toolCall = do
    -- Extract arguments from toolCall
    let args = extractArgs toolCall
    -- Call original tool
    result <- runTool originalTool args
    -- Convert result to Text
    pure $ convertToText result
```

## Running an Agent

### Agent Executor

The `runAgentExecutor` function orchestrates the complete agent execution:

```haskell
runAgentExecutor ::
  Agent a =>
  a ->                              -- The agent
  AgentConfig ->                    -- Configuration
  AgentCallbacks ->                 -- Callbacks
  Text ->                           -- User input/query
  IO (LangchainResult AgentExecutionResult)
```

### Execution Result

```haskell
data AgentExecutionResult = AgentExecutionResult
  { executionFinish :: AgentFinish      -- Final result
  , executionSteps :: [AgentStep]       -- All steps taken
  , executionMetrics :: ExecutionMetrics -- Performance data
  }

data ExecutionMetrics = ExecutionMetrics
  { metricsIterations :: Int            -- Number of iterations
  , metricsExecutionTime :: Double      -- Total time in seconds
  , metricsToolCalls :: Int             -- Number of tool calls
  , metricsSuccess :: Bool              -- Completion status
  }
```

## Complete Example

Here's a complete example using Ollama:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Aeson (Value(..))
import qualified Data.Map as HM
import Data.Ollama.Chat
  ( FunctionDef(..)
  , FunctionParameters(..)
  , InputTool(..)
  )
import qualified Data.Text as T
import Langchain.Agent.Core
import Langchain.Agent.Executor
import Langchain.Agent.ReAct
import Langchain.LLM.Ollama
import Langchain.Tool.Core
import qualified Data.Ollama.Chat as O

-- Define your tool
data AgeFinderTool = AgeFinderTool

instance Tool AgeFinderTool where
  type Input AgeFinderTool = ToolCall
  type Output AgeFinderTool = T.Text
  
  toolName _ = "age_finder"
  toolDescription _ = "Finds the age of a person given their name."
  
  runTool _ (ToolCall _ _ ToolFunction{..}) = do
    if toolFunctionName == "age_finder"
      then do
        case HM.lookup "name" toolFunctionArguments of
          Nothing -> pure "Unknown"
          Just (String name_) -> pure $ getAge name_
          _ -> pure "Unknown"
      else pure "Unknown"
    where
      getAge name = case name of
        "Alice" -> "40"
        "Bob" -> "45"
        "Charlie" -> "35"
        _ -> "Unknown"

main :: IO ()
main = do
  -- 1. Create the LLM
  let llm = Ollama "qwen3:4b" []
  
  -- 2. Wrap tools in ToolAcceptingToolCall
  let tools = [ToolAcceptingToolCall AgeFinderTool]
  
  -- 3. Define tool schema for the LLM (Ollama-specific)
  let paramProp = HM.fromList
        [ ("name", FunctionParameters "string" Nothing Nothing Nothing)
        ]
      functionParams = FunctionParameters
        { parameterType = "object"
        , requiredParams = Just ["name"]
        , parameterProperties = Just paramProp
        , additionalProperties = Just False
        }
      functionDef = FunctionDef
        { functionName = "age_finder"
        , functionDescription = Just "Finds the age of a person given their name."
        , functionParameters = Just functionParams
        , functionStrict = Nothing
        }
      inputTool = InputTool
        { toolType = "function"
        , function = functionDef
        }
  
  -- 4. Create LLM parameters with tool definitions
  let mbOllamaParams = Just $ O.defaultChatOps
            { O.tools = Just [inputTool]
            }
  
  -- 5. Create the agent
  let agent = createReActAgent llm mbOllamaParams tools
  
  -- 6. Run the agent
  result <- runAgentExecutor
    agent
    defaultAgentConfig
    defaultAgentCallbacks
    "What is the age of Alice?"
  
  -- 7. Handle the result
  case result of
    Left err -> putStrLn $ "Error: " <> show err
    Right execResult -> do
      putStrLn $ "Answer: " <> T.unpack (agentOutput $ executionFinish execResult)
      putStrLn $ "Iterations: " <> show (metricsIterations $ executionMetrics execResult)
      putStrLn $ "Tool calls: " <> show (metricsToolCalls $ executionMetrics execResult)
```

## Critical Warnings

### 1. Tool Input/Output Types

Your tools **MUST** have:
- `Input` type = `ToolCall`
- `Output` type = `Text`

This is enforced by the `ToolAcceptingToolCall` wrapper. If your existing tools have different types, you must create new wrapper tools as shown above.

### 2. Tool Definitions in LLM Params

You **MUST** pass tool definitions to the LLM parameters. The exact format depends on your LLM provider:

**For Ollama:**
```haskell
let mbParams = Just $ O.defaultChatOps
      { O.tools = Just [inputTool1, inputTool2, ...]
      }
```

**For OpenAI (when supported):**
```haskell
let mbParams = Just $ defaultOpenAIParams
      { tools = Just [toolDef1, toolDef2, ...]
      }
```

### 3. Tool Lists Must Match

The tool definitions in `LLMParams` **MUST** correspond to the tools in `reactTools`:

```haskell
-- These must match!
let tools = [ToolAcceptingToolCall Tool1, ToolAcceptingToolCall Tool2]
let llmToolDefs = [tool1Def, tool2Def]  -- Corresponding definitions

let agent = createReActAgent llm (Just params{tools = llmToolDefs}) tools
```

If they don't match:
- The LLM might call a tool that isn't in `reactTools` → Runtime error
- A tool in `reactTools` isn't in LLM definitions → LLM won't know it exists

### 4. Tool Name Consistency

The tool name must be consistent across:
1. Your `Tool` instance (`toolName` method)
2. The LLM tool definition (e.g., `functionName` in Ollama)
3. The check inside `runTool` (matching `toolFunctionName`)

```haskell
instance Tool MyTool where
  toolName _ = "my_tool"  -- Must match everywhere!
  
  runTool _ (ToolCall _ _ ToolFunction{..}) = do
    if toolFunctionName == "my_tool"  -- Same name here
      then ...
```

## Using Callbacks

Monitor agent execution with callbacks:

```haskell
let callbacks = defaultAgentCallbacks
      { onAgentAction = \action -> do
          putStrLn $ "Calling tool: " <> show (actionToolCall action)
      , onAgentObservation = \obs -> do
          putStrLn $ "Tool result: " <> T.unpack obs
      , onAgentFinish = \finish -> do
          putStrLn $ "Task complete: " <> T.unpack (agentOutput finish)
      , onAgentError = \err -> do
          putStrLn $ "Error occurred: " <> show err
      }

result <- runAgentExecutor agent defaultAgentConfig callbacks input
```

## Best Practices

1. **Start with few tools**: Begin with 1-2 tools and add more as needed
2. **Clear tool descriptions**: The LLM relies on descriptions to choose tools
3. **Set iteration limits**: Prevent infinite loops with reasonable `maxIterations`
4. **Use callbacks for debugging**: Monitor what the agent is doing
5. **Handle errors in tools**: Return descriptive error messages as `Text`
6. **Validate tool arguments**: Check that required parameters are present
7. **Test tools independently**: Verify tools work before adding to agent

## Troubleshooting

### Agent doesn't call tools
- Check that tools are in LLM params
- Verify tool descriptions are clear
- Ensure LLM model supports tool calling

### "Cannot find tool with name: X"
- Tool name mismatch between LLM definition and `reactTools`
- Check all three places: `toolName`, LLM definition, `runTool` check

### Agent exceeds iteration limit
- Task may be too complex
- Increase `maxIterations` in config
- Check if agent is stuck in a loop (use callbacks to debug)

### Type errors with ToolAcceptingToolCall
- Your tool must have `Input ~ ToolCall` and `Output ~ Text`
- Create a wrapper tool if needed

## Next Steps

- Learn about [Tools](/tools) in detail
- Explore [LLM integration](/llm) options
- See more [Examples](/examples)

