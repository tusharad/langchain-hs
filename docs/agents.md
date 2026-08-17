# Advanced Agent Architectures

LangChain-HS features state-of-the-art agent architectures inspired by langchain4j and designed with pure functional principles.

---

## 1. Plan-and-Execute Agent
Separates complex goal reasoning into structured step planning and sequential execution:

```haskell
import Langchain.Prelude

let planner = newOllama "qwen2.5:7b" "http://localhost:11434"
let executor = planner
let agent = newPlanAndExecuteAgent planner executor Nothing

result <- runPlanAndExecute agent "Write a Haskell CLI tool that counts words in files"
```

## 2. OpenAI / Ollama Functions Agent
Supports parallel function calling and tool results:

```haskell
let agent = newFunctionsAgent model [calculatorTool, fileSystemTool] (Just "System prompt")
result <- runFunctionsAgent agent "What is 42 * 100?"
```

## 3. Multi-Agent Supervisor Teams
```haskell
let coder = SpecialistAgent "Coder" "Writes code" ["haskell"] (\task -> ...)
let reviewer = SpecialistAgent "Reviewer" "Reviews code" ["testing"] (\task -> ...)
let team = newSupervisorTeam supervisorModel [coder, reviewer]

ans <- runSupervisorTeam team "Implement sorting algorithm with test coverage"
```

## 4. Multi-Agent Debate
```haskell
let debaterA = Debater "AdvocateA" "Perspective A" modelA
let debaterB = Debater "AdvocateB" "Perspective B" modelB
let cfg = defaultDebateConfig "Should AI architectures be pure?"

(verdict, rounds) <- runDebate cfg [debaterA, debaterB] moderatorModel
```
