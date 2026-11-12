# Changelog for langchain-hs-ollama

## 0.0.6.2 - 2026-11-12

- Coordinated monorepo release alignment to `0.0.6.2`.
- Forward streamed tool calls from `ollama-haskell` in `ChatModel.stream` as `LLMChunk "" (Just toolCall)`.
- Updated `langchain-hs-core` dependency bound to `>= 0.0.6.2 && < 0.0.7`.

## 0.0.6.1 - 2026-10-25

- Upgrade dependency `ollama-haskell` to `>= 0.4.2.0 && < 0.5`.
- Update message and tool call conversions for `ollama-haskell-0.4.2.0`.

## 0.0.6.0 - 2026-10-02

- Initial release. Split from the monolithic `langchain-hs` package.
- Provides `Langchain.Provider.Ollama` and `Langchain.Embeddings.Ollama`.
