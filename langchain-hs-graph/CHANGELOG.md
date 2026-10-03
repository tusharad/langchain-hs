# Changelog for `langchain-hs-graph`

All notable changes to this package will be documented in this file.

## 0.0.6.0 - 2026-10-02

- Updated bounds for GHC 9.12 and modern Stackage LTS releases.
- Standalone stack configuration and verified test suite.

## 0.0.5.0 - 2026-09-10

- Initial standalone Hackage release of `langchain-hs-graph`.
- Cyclic graph orchestration engine (`StateGraph s m`).
- Pure monoidal state merge reducers (`StateReducer s`).
- In-memory STM `TVar` checkpointer and persistent SQLite checkpointer.
- Human-in-the-Loop (`HITL`) interruption and resumption.
- Parallel concurrent node execution via `async`.
- Multi-Agent coordination: Supervisor teams, debate loops, and voting classifiers.
- Graphviz DOT export for visual workflow inspection.
