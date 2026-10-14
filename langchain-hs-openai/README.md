# langchain-hs-openai

OpenAI ChatModel provider and embeddings for [langchain-hs](https://hackage.haskell.org/package/langchain-hs).

Supports OpenAI and any OpenAI-compatible endpoint (OpenRouter, Fireworks,
Together AI). Does not depend on Ollama or Gemini packages.

## Installation

```yaml
# stack.yaml extra-deps
- langchain-hs-openai-0.0.6.0
```

```yaml
# package.yaml / .cabal
dependencies:
  - langchain-hs-openai
```

## Usage

```haskell
import Langchain.Provider.OpenAI
import Control.Monad.Except (runExceptT)

main :: IO ()
main = do
  let model = newOpenAI "sk-..." "gpt-4o"
  res <- runExceptT $ invoke model [userMessage "Hello"] Nothing
  print res
```
