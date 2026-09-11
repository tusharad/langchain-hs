# `langchain-hs-graph`

> Stateful agent orchestration and cyclic graph workflows for `langchain-hs` (LangGraph in Haskell).

`langchain-hs-graph` provides cyclic state machines, pure monoidal state reducers, in-memory STM checkpointers, persistent SQLite checkpointers, Human-in-the-Loop (`HITL`) interrupts, and multi-agent coordination patterns.

## Key Primitives

- **`StateGraph s m`**: Cyclic workflow builder with typed nodes, directed edges, and conditional branches.
- **`StateReducer s`**: Pure binary state merge reducer (`s -> s -> s`) satisfying monoid associativity laws.
- **Checkpointers**:
  - `MemoryCheckpointer`: In-memory thread-safe state persistence using STM `TVar`.
  - `SQLiteCheckpointer`: Persistent state checkpointer for production runs.
- **Human-in-the-Loop (`HITL`)**: Node interruption before execution, inspection/modification, and resumed execution via `resumeGraph`.
- **Parallel Nodes**: Concurrent node evaluation using `async`.
- **Multi-Agent Architectures**: Supervisor teams with capability routing, multi-agent debate, and majority voting.

## Installation

```cabal
build-depends: langchain-hs-graph >= 0.0.5 && < 0.0.6
```

## License

MIT License. See [LICENSE](LICENSE).
