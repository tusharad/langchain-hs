# Agent Module for LangChain Haskell

This directory contains the Agent system for langchain-hs, providing autonomous systems that use language models to determine actions dynamically.

## Overview

Agents are systems that use an LLM as a reasoning engine to decide:
- What actions to take
- In what order to execute them
- When the task is complete

Unlike chains (predetermined sequences), agents adapt their behavior based on observations from previous steps.

## Architecture

### Core Components

```
┌─────────────────────────────────────────────────────────┐
│                    Agent System                         │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  ┌──────────┐    ┌──────────────┐    ┌─────────────┐ │
│  │  Agent   │───>│   Executor   │───>│   Tools     │ │
│  │  (Brain) │<───│   (Engine)   │<───│  (Actions)  │ │
│  └──────────┘    └──────────────┘    └─────────────┘ │
│       │                  │                    │        │
│       │                  │                    │        │
│       v                  v                    v        │
│   Planning           Loop Control        Execution     │
│   Reasoning         Error Handling      Results        │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

## Modules

### Core.hs
Defines the fundamental abstractions:
- `Agent` typeclass - Interface for all agent implementations
- `AgentAction` - Represents an action to execute
- `AgentFinish` - Represents completion
- `AgentStep` - Combines action + observation
- `AgentState` - Current execution state
- `AgentConfig` - Configuration options
- `AgentCallbacks` - Lifecycle hooks

### Executor.hs
Implements the agent execution loop:
- Main loop orchestration
- Error handling and recovery
- Iteration/timeout management
- State management
- Metrics collection

### ReAct.hs
Implements the ReAct (Reasoning + Acting) pattern:
- LLM-based planning
- Tool routing
- Output parsing
- Prompt engineering

### Tools.hs
Tool management utilities:
- Tool registry for storing available tools
- Tool wrapping for type conversion
- Safe execution wrappers
- Timeout and retry logic

## Agent Flow

```
┌───────────────┐
│ User Question │
└───────┬───────┘
        │
        v
┌─────────────────────┐
│  Initialize Agent   │
│  - Set up state     │
│  - Load tools       │
└─────────┬───────────┘
          │
          v
    ┌─────────────┐
    │   PLAN      │<───────┐
    │ (Use LLM)   │        │
    └─────┬───────┘        │
          │                │
    ┌─────▼──────────┐     │
    │ Action or      │     │
    │ Finish?        │     │
    └─────┬──────────┘     │
          │                │
     ┌────┴────┐           │
     │         │           │
     v         v           │
┌─────────┐  ┌─────────┐  │
│ ACTION  │  │ FINISH  │  │
│         │  │         │  │
└────┬────┘  └────┬────┘  │
     │            │        │
     v            v        │
┌──────────┐  ┌────────┐  │
│ Execute  │  │ Return │  │
│ Tool     │  │ Result │  │
└────┬─────┘  └────────┘  │
     │                     │
     v                     │
┌──────────┐               │
│ Observe  │               │
│ Result   │               │
└────┬─────┘               │
     │                     │
     └─────────────────────┘
        (Loop back)
```

## Usage Examples

### Basic Agent

```haskell
import Langchain.Agent.Core
import Langchain.Agent.ReAct
import Langchain.Agent.Executor
import Langchain.Agent.Tools
import Langchain.Tool.Calculator
import Langchain.LLM.Ollama

main :: IO ()
main = do
  -- Create LLM
  let llm = Ollama "llama3.2" []
  
  -- Wrap tools
  let calculator = wrapToolWithTransform
        CalculatorTool
        (Just . id)
        (either id (T.pack . show))
  
  -- Create registry
  let tools = createToolRegistry [calculator]
  
  -- Create agent
  let agent = createReActAgent llm tools
  
  -- Run agent
  result <- runAgentExecutor
    agent
    defaultAgentConfig
    defaultAgentCallbacks
    "What is 25 * 4 + 10?"
  
  case result of
    Left err -> putStrLn $ "Error: " <> show err
    Right execResult ->
      putStrLn $ agentOutput $ executionFinish execResult
```

### With Callbacks

```haskell
let callbacks = defaultAgentCallbacks
      { onAgentStart = \input ->
          putStrLn $ "Starting: " <> T.unpack input
      , onAgentAction = \action ->
          putStrLn $ "Tool: " <> T.unpack (actionTool action)
      , onAgentFinish = \finish ->
          putStrLn $ "Done: " <> T.unpack (agentOutput finish)
      }

result <- runAgentExecutor agent config callbacks input
```

### Custom Configuration

```haskell
let config = defaultAgentConfig
      { maxIterations = 10           -- Limit steps
      , maxExecutionTime = Just 60   -- Timeout after 60s
      , verboseLogging = True        -- Debug output
      , handleParsingErrors = True   -- Auto-recover
      , returnIntermediateSteps = True  -- Return all steps
      }
```

## Key Features

### 1. Type-Safe Tool Integration
Tools are wrapped with proper type conversions, ensuring type safety while maintaining flexibility.

### 2. Robust Error Handling
- Automatic parsing error recovery
- Tool execution error handling
- Timeout management
- Graceful degradation

### 3. Observability
- Comprehensive callbacks
- Execution metrics
- Intermediate step tracking
- Verbose logging mode

### 4. Flexibility
- Custom agents via typeclass implementation
- Configurable execution parameters
- Pluggable tool systems
- Custom prompts

### 5. Production-Ready
- Iteration limits
- Execution timeouts
- Resource cleanup
- Error recovery

## Agent Patterns

### ReAct (Implemented)
Alternates between reasoning and acting:
```
Thought -> Action -> Observation -> Thought -> ...
```

### Future Patterns
- **Plan-and-Execute**: Plan all steps upfront, then execute
- **Self-Ask**: Decompose questions into sub-questions
- **Tool-Using**: Specialized tool selection and composition
- **Conversational**: Multi-turn dialogues with memory

## Tool Development

### Simple Tool

```haskell
let myTool = AgentTool
      { agentToolName = "greeter"
      , agentToolDescription = "Greets a person"
      , agentToolExecutor = \name ->
          return $ Right $ "Hello, " <> name
      , agentToolSchema = Nothing
      }
```

### Tool from Typeclass

```haskell
-- Assuming you have a Tool instance
data MyTool = MyTool

instance Tool MyTool where
  type Input MyTool = Text
  type Output MyTool = Either String Text
  
  toolName _ = "my_tool"
  toolDescription _ = "Does something useful"
  runTool _ input = -- implementation

-- Wrap for agent use
let wrapped = wrapToolWithTransform
      MyTool
      (Just . id)
      (either id id)
```

## Testing

The module includes comprehensive tests:
- Unit tests for core functionality
- Integration tests for agent execution
- Tool wrapping tests
- Prompt parsing tests

Run tests:
```bash
stack test --test-arguments "-p Agent"
```

## Performance Considerations

### Iteration Limits
Set reasonable `maxIterations` to prevent runaway loops:
```haskell
config { maxIterations = 10 }  -- Good for most tasks
```

### Timeouts
Use `maxExecutionTime` for long-running operations:
```haskell
config { maxExecutionTime = Just 300 }  -- 5 minutes
```

### Tool Optimization
- Cache tool results when possible
- Use async execution for independent tools
- Implement tool timeout wrappers

## Debugging

### Enable Verbose Logging
```haskell
config { verboseLogging = True }
```

### Capture Intermediate Steps
```haskell
config { returnIntermediateSteps = True }
```

### Use Callbacks
```haskell
callbacks { onAgentAction = print }
```

## Common Patterns

### Research Agent
```haskell
tools = [search, wikipedia, calculator]
agent = createReActAgent llm tools
```

### Data Processing Agent
```haskell
tools = [fileReader, dataParser, calculator]
agent = createReActAgent llm tools
```

### API Integration Agent
```haskell
tools = [apiCaller, jsonParser, validator]
agent = createReActAgent llm tools
```

## Best Practices

1. **Tool Design**
   - Clear, descriptive names
   - Comprehensive descriptions
   - Predictable error handling
   - Fast execution

2. **Prompt Engineering**
   - Clear instructions
   - Good examples
   - Format specifications
   - Error guidance

3. **Configuration**
   - Reasonable iteration limits
   - Appropriate timeouts
   - Enable error recovery
   - Use callbacks for monitoring

4. **Error Handling**
   - Handle tool failures gracefully
   - Provide useful error messages
   - Enable parsing error recovery
   - Log for debugging

## References

- [ReAct Paper](https://arxiv.org/abs/2210.03629)
- [LangChain Python Agents](https://docs.langchain.com/oss/python/langchain/agents)
- [OpenAI Agents](https://openai.github.io/openai-agents-python/)
- [Agent Documentation](../../../../docs/docs/concepts/Agents.md)

## Contributing

When adding new agent types:
1. Implement the `Agent` typeclass
2. Add comprehensive tests
3. Document usage examples
4. Update this README

## License

MIT License - See LICENSE file for details


