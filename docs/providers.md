# Provider Integrations in LangChain-HS

LangChain-HS provides first-class support for local and cloud model providers.

---

## 1. Ollama (Local & Free)

```haskell
import Langchain.Prelude

let ollama = newOllama "qwen2.5:7b" "http://localhost:11434"
```

## 2. OpenAI

```haskell
let openai = newOpenAI "gpt-4o" (Just "your-api-key")
```

## 3. Google Gemini

```haskell
let gemini = newGemini "gemini-1.5-pro" (Just "your-api-key")
```
