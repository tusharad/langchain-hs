# Changelog for `langchain-hs-core`

All notable changes to this package will be documented in this file.

## 0.0.6.0 - 2026-10-02

- Updated bounds for GHC 9.12 and modern Stackage LTS releases.
- Standalone stack configuration and verified test suite.

## 0.0.5.0 - 2026-09-10

- Initial standalone Hackage release of `langchain-hs-core`.
- Zero-dependency pure core architecture.
- First-class `RunnableTree` GADT AST with sequential (`|>>`), parallel (`&>&`), and fallback (`>>>#`) operators.
- `ChatModel` effect-polymorphic interface.
- Multi-modal `ContentBlock` and message structures.
- Conduit-based `StreamEvent` and streaming protocol.
- Decoupled `LangchainT env m a` transformer.
- Typed `Tool` and `FunctionDefinition`.
