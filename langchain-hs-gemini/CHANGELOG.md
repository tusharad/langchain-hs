# Changelog for langchain-hs-gemini

## 0.0.6.2 - 2026-11-14

> **Note**: `0.0.6.1` was skipped globally after `langchain-hs-ollama-v0.0.6.1`.

- Added incremental tool-call emission during SSE streaming in `Langchain.Provider.Gemini` (`ChatModel.stream`).
- Consolidates Interactions v1 Servant bindings (`Gemini.Client.*`) and `Langchain.Embeddings.Gemini` under aligned `0.0.6.2`.
- Updated `langchain-hs-core` dependency bound to `>= 0.0.6.2 && < 0.0.7`.

## 0.0.6.0 - 2026-10-02

- Initial release. Split from the monolithic `langchain-hs` package.
- Provides `Langchain.Provider.Gemini`.
