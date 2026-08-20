# Observability, Telemetry & Guardrails

LangChain-HS provides production-grade observability and safety primitives.

---

## 1. OpenTelemetry Spans

```haskell
import Langchain.Prelude

tracer <- newOTelTracer Nothing
res <- withSpan tracer "agent_execution" Nothing ClientSpan mempty $ do
  -- execute LLM or tool calls
  pure ()

jsonTraces <- exportSpansJson tracer
```

## 2. Structured Logging

```haskell
logger <- newInMemoryLogger InfoLevel
logInfo (stderrLogger InfoLevel) "Agent" "Agent execution complete"
```

## 3. Circuit Breaker for Resilient Degradation

```haskell
cb <- newCircuitBreaker "openai-api" defaultCircuitConfig
result <- withCircuitBreaker cb (invoke model msgs Nothing)
```

## 4. Input & Output Guardrails

```haskell
let safetyRail = contentSafetyGuardrail ["forbidden_word"]
let lengthRail = outputLengthGuardrail 500
let rails = composeGuardrails [safetyRail, lengthRail]

safeResult <- withGuardrails rails (\prompt -> invoke model [userMessage prompt] Nothing) "User question"
```
