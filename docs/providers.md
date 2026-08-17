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

## 3. Anthropic Claude

```haskell
let anthropic = newAnthropic "claude-3-5-sonnet-20241022" (Just "your-api-key")
```

## 4. DeepSeek

```haskell
let deepseek = newDeepSeek "deepseek-chat" (Just "your-api-key")
```

## 5. Google Gemini

```haskell
let gemini = newGemini "gemini-1.5-pro" (Just "your-api-key")
```
