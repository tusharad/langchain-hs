---
sidebar_position: 1
---

# langchain-hs Documentation

Langchain-hs is the Haskell implementation of [LangChain](https://www.langchain.com/).

## Welcome to langchain-hs

Langchain-hs brings the power of LangChain to Haskell, enabling type-safe LLM application development with:

- Composable pipelines using Haskell's type system
- Closely following with Official LangChain concepts
- Performance through functional programming patterns
- Support for Ollama, OpenAI and custom LLM integrations

## Getting Started

Check out the [Quickstart](getting-started/quick_start) guide to get started with langchain-hs.

## Components 

This documentation provides an overview of the core components of langchain-hs. Each component is designed to be modular, composable and interchangeable, allowing you to build complex applications with ease. 

Langchain-hs is built around the core concepts of LangChain; You will find documentation for each components including:

- **[LLMs](concepts/LLM)**: Language models that can be used for various tasks.
- **[Document Loader](concepts/DocumentLoader)**: Components for loading and processing documents from various sources.
- **[Embedding](concepts/Embeddings)**: Components for generating vector representations of text.
- **[Memory](concepts/Memory)**: Mechanisms for storing and retrieving chat history and other contextual information.
- **[Output Parser](concepts/OutputParser)**: Components for parsing and processing the output of LLMs.
- **[Callback](concepts/Callback)**: Mechanisms for monitoring the execution of LLMs.
- **[Prompt](concepts/PromptTemplate)**: Templates for generating input for LLMs.
- **[Vector Store](concepts/VectorStore)**: Mechanism for storing and retrieving document embeddings.
- **[Retriever](concepts/Retriever)**: Generic interface for retrieving documents from a vector store.
- **[Text Splitter](concepts/TextSplitter)**: Components for splitting text into smaller chunks for processing.
- **[Tool](concepts/Tool)**: External functions or APIs that can be called by agents to perform specific tasks.
- **[Agent](concepts/Agents)**: Components that can make decisions based on the output of LLMs and other components.
- **[Chain](concepts/Chain)**: Composable sequences of operations that can be executed in a specific order.

## API Reference

Please refer to the [Hackage documentation](https://hackage.haskell.org/package/langchain-hs) for the API reference.

## Additional support

Feel free to raise issues or ask questions in the [GitHub repository](https://github.com/tusharad/langchain-hs/issues). We welcome contributions and feedback from the community. You can also join the [Langchain-hs Discord](https://discord.gg/swpKq59RJA) for discussions and support.

**Current Status** : Actively developed with expanding feature set. Some functionalities maybe a bit unstable. Please report any issues you encounter.
