# Agents

## Overview

Agents are autonomous systems that use language models to determine which actions to take and in what order. Unlike chains which execute a predetermined sequence of steps, agents use an LLM as a reasoning engine to decide the flow of execution dynamically.

## Core Concepts

### Agent

An agent is a system that:
1. **Plans** - Decides what action to take next based on the current state
2. **Acts** - Executes tools to accomplish tasks
3. **Observes** - Receives feedback from tool execution
4. **Iterates** - Repeats the process until the task is complete

### Key Components

#### AgentAction
Represents a single action the agent wants to take:
```haskell
data AgentAction = AgentAction
  { actionTool :: Text          -- Tool to execute
  , actionToolInput :: Text     -- Input for the tool
  , actionLog :: Text           -- Reasoning/thoughts
  , actionMetadata :: Map Text Text
  }
```

#### AgentFinish
Represents the final result when the agent completes:
```haskell
data AgentFinish = AgentFinish
  { agentOutput :: Text         -- Final answer
  , finishMetadata :: Map Text Text
  , finishLog :: Text           -- Final thoughts
  }
```

#### AgentStep
Combines an action with its observation:
```haskell
data AgentStep = AgentStep
  { stepAction :: AgentAction
  , stepObservation :: Text
  , stepTimestamp :: UTCTime
  }
```

## Agent Types

### ReAct Agent

The ReAct (Reasoning + Acting) agent alternates between reasoning about what to do and taking actions. It follows this pattern:

```
Thought: [reasoning about what to do]
Action: [tool to use]
Action Input: [input for the tool]
Observation: [result from the tool]
... (repeat as needed)
Thought: I now know the final answer
Final Answer: [the answer]
```

#### Creating a ReAct Agent

```haskell
import Langchain.Agent.Core
import Langchain.Agent.ReAct
import Langchain.Agent.Tools
import Langchain.Tool.Calculator
import Langchain.Tool.WebScraper
import Langchain.LLM.Ollama

-- Create the LLM
let llm = Ollama "llama3.2" []

-- Create tools
let calculator = wrapToolWithTransform 
      CalculatorTool
      (Just . id)
      (either id (T.pack . show))
    
    webScraper = wrapToolWithTransform
      WebScraper
      (Just . id)
      (either id id)

-- Create tool registry
let tools = createToolRegistry [calculator, webScraper]

-- Create agent
let agent = createReActAgent llm tools
```

## Agent Executor

The `AgentExecutor` orchestrates the agent execution loop:

```haskell
import Langchain.Agent.Executor

-- Configure execution
let config = defaultAgentConfig
      { maxIterations = 10
      , verboseLogging = True
      , handleParsingErrors = True
      }

-- Set up callbacks
let callbacks = defaultAgentCallbacks
      { onAgentAction = \action -> 
          putStrLn $ "Using tool: " <> actionTool action
      , onAgentFinish = \finish ->
          putStrLn $ "Final answer: " <> agentOutput finish
      }

-- Run the agent
result <- runAgentExecutor agent config callbacks "What is 25 * 4?"

case result of
  Left err -> putStrLn $ "Error: " <> show err
  Right execResult -> do
    putStrLn $ agentOutput $ executionFinish execResult
    putStrLn $ "Iterations: " <> show (metricsIterations $ executionMetrics execResult)
```

## Configuration

### AgentConfig

Control agent execution behavior:

```haskell
data AgentConfig = AgentConfig
  { maxIterations :: Int              -- Max steps (default: 15)
  , maxExecutionTime :: Maybe Int     -- Max time in seconds
  , returnIntermediateSteps :: Bool   -- Include all steps
  , handleParsingErrors :: Bool       -- Auto-recover from errors
  , verboseLogging :: Bool            -- Enable detailed logs
  }
```

### Callbacks

Hook into agent lifecycle events:

```haskell
data AgentCallbacks = AgentCallbacks
  { onAgentStart :: Text -> IO ()
  , onAgentAction :: AgentAction -> IO ()
  , onAgentObservation :: Text -> IO ()
  , onAgentFinish :: AgentFinish -> IO ()
  , onAgentError :: LangchainError -> IO ()
  , onAgentStep :: AgentStep -> IO ()
  }
```

## Tools

Tools are functions that agents can use to interact with the world.

### Creating Tools

#### Using existing Tool instances

```haskell
import Langchain.Tool.Calculator

-- Wrap a tool for use with agents
let calculator = wrapToolWithTransform
      CalculatorTool
      (Just . id)              -- Parse input
      (either id (T.pack . show))  -- Format output
```

#### Creating custom tools

```haskell
-- Define a custom tool
let weatherTool = AgentTool
      { agentToolName = "weather"
      , agentToolDescription = "Gets weather for a location"
      , agentToolExecutor = \location -> do
          -- Call weather API
          return $ Right $ "Weather in " <> location <> ": Sunny"
      , agentToolSchema = Nothing
      }

-- Add to registry
let tools = createToolRegistry [weatherTool, calculator]
```

### Tool Registry

Manage available tools:

```haskell
import Langchain.Agent.Tools

-- Create registry
let registry = createToolRegistry [tool1, tool2, tool3]

-- Add a tool
let registry' = addTool newTool registry

-- Remove a tool
let registry'' = removeTool "tool_name" registry'

-- Look up a tool
case lookupTool "calculator" registry of
  Just executor -> executor "2+2"
  Nothing -> putStrLn "Tool not found"
```

## Error Handling

Agents include robust error handling:

### Automatic Recovery

When `handleParsingErrors = True`, the agent will:
- Catch parsing errors
- Provide feedback to the LLM
- Give it another chance to respond correctly

### Manual Error Handling

```haskell
result <- runAgentExecutor agent config callbacks input

case result of
  Left err -> do
    -- Handle error
    case getCategory err of
      AgentError -> putStrLn "Agent error"
      ToolError -> putStrLn "Tool execution error"
      _ -> putStrLn "Other error"
  Right execResult -> do
    -- Process result
    processResult execResult
```

## Best Practices

### 1. Choose Appropriate Tools

Select tools that:
- Are reliable and well-tested
- Have clear, unambiguous names
- Provide good descriptions
- Handle errors gracefully

### 2. Set Reasonable Limits

```haskell
let config = defaultAgentConfig
      { maxIterations = 10      -- Prevent infinite loops
      , maxExecutionTime = Just 60  -- Timeout after 1 minute
      }
```

### 3. Use Verbose Logging During Development

```haskell
let config = defaultAgentConfig
      { verboseLogging = True   -- See what's happening
      }
```

### 4. Monitor with Callbacks

```haskell
let callbacks = defaultAgentCallbacks
      { onAgentAction = logAction
      , onAgentError = handleError
      , onAgentFinish = recordMetrics
      }
```

### 5. Provide Clear Instructions

The system prompt should clearly explain:
- Available tools and their purposes
- Expected format for actions
- When to provide a final answer

## Advanced Usage

### Custom Agents

Implement the `Agent` typeclass for full control:

```haskell
data MyCustomAgent = MyCustomAgent { ... }

instance Agent MyCustomAgent where
  plan agent state = do
    -- Custom planning logic
    ...
  
  getTools agent = [...]
  
  executeTool agent toolName input = do
    -- Custom tool execution
    ...
```

### Agent State Management

Access and modify agent state:

```haskell
-- Create initial state
let initialState = createInitialState "user query"
      { agentChatHistory = customHistory
      , agentScratchpad = existingSteps
      }

-- Run with custom state
result <- runAgentExecutorWithState agent config callbacks initialState
```

### Intermediate Steps

Retrieve all steps taken by the agent:

```haskell
let config = defaultAgentConfig
      { returnIntermediateSteps = True
      }

result <- runAgentExecutor agent config callbacks input

case result of
  Right execResult -> do
    -- Access all steps
    let steps = executionSteps execResult
    mapM_ printStep steps
```

## Examples

### Simple Calculation Agent

```haskell
import Langchain.Agent.ReAct
import Langchain.Agent.Executor
import Langchain.Tool.Calculator

main :: IO ()
main = do
  let llm = Ollama "llama3.2" []
      calculator = wrapToolWithTransform CalculatorTool (Just . id) (either id (T.pack . show))
      tools = createToolRegistry [calculator]
      agent = createReActAgent llm tools
  
  result <- runAgentExecutor 
    agent 
    defaultAgentConfig 
    defaultAgentCallbacks
    "What is (25 + 15) * 2?"
  
  case result of
    Left err -> putStrLn $ "Error: " <> show err
    Right execResult -> 
      putStrLn $ "Answer: " <> T.unpack (agentOutput $ executionFinish execResult)
```

### Multi-Tool Research Agent

```haskell
import Langchain.Tool.DuckDuckGo
import Langchain.Tool.WikipediaTool
import Langchain.Tool.Calculator

main :: IO ()
main = do
  let llm = Ollama "llama3.2" []
      
  -- Create multiple tools
  let search = wrapToolWithTransform DuckDuckGo (Just . id) (either id id)
      wiki = wrapToolWithTransform WikipediaTool (Just . id) (either id id)
      calc = wrapToolWithTransform CalculatorTool (Just . id) (either id (T.pack . show))
      
      tools = createToolRegistry [search, wiki, calc]
      agent = createReActAgent llm tools
  
  result <- runAgentExecutor
    agent
    (defaultAgentConfig { maxIterations = 15 })
    defaultAgentCallbacks
    "Research Albert Einstein and calculate what year he would turn 150"
  
  case result of
    Left err -> putStrLn $ "Error: " <> show err
    Right execResult ->
      putStrLn $ T.unpack (agentOutput $ executionFinish execResult)
```

## Troubleshooting

### Agent Gets Stuck in Loop

**Problem**: Agent repeats the same actions
**Solutions**:
- Reduce `maxIterations`
- Improve tool descriptions
- Use a more capable LLM
- Add loop detection in custom agents

### Parsing Errors

**Problem**: Agent output doesn't match expected format
**Solutions**:
- Enable `handleParsingErrors = True`
- Improve system prompt clarity
- Use few-shot examples
- Try a different LLM model

### Tool Execution Failures

**Problem**: Tools return errors frequently
**Solutions**:
- Add error handling in tools
- Validate tool inputs
- Provide better tool descriptions
- Use `executeSafely` wrapper

### Slow Execution

**Problem**: Agent takes too long
**Solutions**:
- Reduce `maxIterations`
- Set `maxExecutionTime`
- Use faster LLM models
- Cache tool results
- Optimize tool implementations

## References

- [ReAct Paper](https://arxiv.org/abs/2210.03629)
- [LangChain Python Agents](https://docs.langchain.com/oss/python/langchain/agents)
- [OpenAI Agents](https://openai.github.io/openai-agents-python/)

## See Also

- [Tools](./Tool.md) - Creating and using tools
- [LLM](./LLM.md) - Language model integration
- [Memory](./Memory.md) - Conversation memory for agents
- [Runnable](./Runnable.md) - Composing agent pipelines


