# langchain-hs-ollama

Ollama ChatModel provider and embeddings for [langchain-hs](https://hackage.haskell.org/package/langchain-hs).

Provides `Langchain.Provider.Ollama` and `Langchain.Embeddings.Ollama` without
pulling in OpenAI or Gemini dependencies.

## Installation

```yaml
# stack.yaml extra-deps
- langchain-hs-ollama-0.0.6.0
```

```yaml
# package.yaml / .cabal
dependencies:
  - langchain-hs-ollama
```

## Usage

```haskell
import Langchain.Provider.Ollama
import Control.Monad.Except (runExceptT)

main :: IO ()
main = do
  model <- newOllama "llama3" defaultOllamaConfig
  res   <- runExceptT $ invoke model [userMessage "Hello"] Nothing
  print res
```
