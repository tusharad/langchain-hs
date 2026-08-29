---
title: Observability & Resilience
description: OpenTelemetry distributed spans, structured JSON logging, circuit breakers, and rate limiters.
category: Core Concepts
---

## OpenTelemetry Distributed Tracing

Track distributed agent workflows, tool invocations, and LLM latency using OpenTelemetry-compatible spans:

```haskell
import Langchain.Prelude
import qualified Data.Map.Strict as Map

otelExample :: IO ()
otelExample = do
  tracer <- newOTelTracer Nothing
  
  -- Wrap operations in hierarchical OpenTelemetry spans
  _ <- withSpan tracer "agent_workflow" Nothing ServerSpan 
                (Map.singleton "workflow.name" "customer_support") $ do
    
    -- Sub-span for LLM call
    withSpan tracer "llm_invoke" Nothing ClientSpan 
             (Map.singleton "model" "qwen2.5:7b") $ do
      invoke model [userMessage "Hello!"]

  -- Export span traces as JSON for Datadog, Jaeger, or Honeycomb
  jsonTrace <- exportSpansJson tracer
  putStrLn jsonTrace
```

---

## Structured Contextual Logging

`langchain-hs` provides structured JSON logging with custom log handlers:

```haskell
-- In-memory log capture for assertions and debugging
logger <- newInMemoryLogger
logInfo logger "Agent started with 3 tools" (Map.singleton "agent_id" "agent-12")

-- Or standard stderr logging
logError stderrLogger "Failed to connect to MCP server" (Map.singleton "port" "8080")
```

---

## Circuit Breakers & Exponential Retries

Protect your application from upstream LLM outages or rate limits using the three-state `CircuitBreaker`:

```
   ┌─────────┐   Failures > threshold   ┌────────┐
   │ Closed  ├─────────────────────────►│  Open  │
   └────▲────┘                          └───┬────┘
        │                                   │
        │ Success                           │ Cooldown elapsed
        │                                   ▼
   ┌────┴───────────────────────────────────────┐
   │                 Half-Open                  │
   └────────────────────────────────────────────┘
```

```haskell
-- Configure Circuit Breaker: 5 failures trips breaker, 30s cooldown
let cbConfig = CircuitBreakerConfig
      { failureThreshold = 5
      , resetTimeoutSeconds = 30
      , halfOpenSuccessThreshold = 2
      }

cb <- newCircuitBreaker cbConfig

-- Wrap risky calls with circuit protection
safeResult <- withCircuitBreaker cb (invoke model messages)
```

---

## Token Cost Accounting

Calculate token expenditure and pricing across providers automatically:

```haskell
let pricing = getStandardPricing "gpt-4o"
let cost = calculateCost pricing (TokenUsage 1500 450)
putStrLn ("Estimated Call Cost: $" ++ show (costInUSD cost))
```
