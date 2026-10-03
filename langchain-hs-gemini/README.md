# langchain-hs-gemini

Google Gemini ChatModel provider for [langchain-hs](https://hackage.haskell.org/package/langchain-hs).

Supports multi-modal content, streaming via SSE, and function calling.
Does not depend on OpenAI or Ollama packages.

## Installation

```yaml
# stack.yaml extra-deps
- langchain-hs-gemini-0.0.6.0
```

```yaml
# package.yaml / .cabal
dependencies:
  - langchain-hs-gemini
```

## Usage

```haskell
import Langchain.Provider.Gemini
import Control.Monad.Except (runExceptT)

main :: IO ()
main = do
  let model = newGemini "AIza..." "gemini-2.0-flash" Nothing
  res <- runExceptT $ invoke model [userMessage "Hello"] Nothing
  print res
```
