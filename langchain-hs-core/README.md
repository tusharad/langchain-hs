# `langchain-hs-core`

> Zero-dependency pure core of the `langchain-hs` ecosystem.

`langchain-hs-core` provides pure GADT abstract syntax trees, effect-polymorphic chat models, streaming protocols, tools, and decoupled monads without ANY HTTP or network dependencies.

## Key Primitives

- **`RunnableTree m a b`**: Pure GADT abstract syntax tree for pipeline composition via `|>>` (sequential), `&>&` (parallel), and `>>>#` (fallback).
- **`ChatModel m`**: Effect-polymorphic typeclass for LLMs.
- **`ContentBlock`**: Multi-modal message content (text, image, tool calls, tool results).
- **`Tool` & `FunctionDefinition`**: Strongly-typed tool execution and JSON parameter schemas.
- **`StreamEvent` & `LLMChunk`**: Conduit-based incremental token streaming.
- **`LangchainT env m a`**: Decoupled reader monad transformer parameterized over custom environment `env`.

## Installation

```cabal
build-depends: langchain-hs-core >= 0.0.5 && < 0.0.6
```

## License

MIT License. See [LICENSE](LICENSE).
