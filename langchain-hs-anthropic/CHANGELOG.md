# Changelog for `langchain-hs-anthropic`

## 0.0.6.2 - 2026-11-14

- Initial release of native Anthropic Messages API (`/v1/messages`) provider for `langchain-hs`.
- Added `Langchain.Provider.Anthropic` implementing `ChatModel`, `ToolBinder`, and `CacheableChatModel`.
- Added SSE streaming support (`text_delta` and fragmented `input_json_delta` accumulation, 0-arg tool handling, and `TokenUsage` reporting).
- Added prompt caching (`withPromptCaching`), extended thinking budgets (`withThinkingBudget`), and token limits (`withMaxTokens`).

