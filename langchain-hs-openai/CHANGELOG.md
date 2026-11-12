# Changelog for langchain-hs-openai

## 0.0.6.2 - 2026-11-12

> **Note**: `0.0.6.1` was skipped globally after `langchain-hs-ollama-v0.0.6.1`.

- Added incremental SSE tool-call delta accumulation and `LLMChunk "" (Just toolCall)` emission in `ChatModel.stream`.
- Coordinated monorepo release alignment to `0.0.6.2` and updated `langchain-hs-core` dependency bound to `>= 0.0.6.2 && < 0.0.7`.

## 0.0.6.0 - 2026-10-02

- Initial release. Split from the monolithic `langchain-hs` package.
- Provides `Langchain.Provider.OpenAI` and `Langchain.Embeddings.OpenAI`.
