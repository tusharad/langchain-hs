module Main (main) where

import Test.Tasty

import qualified Test.Langchain.Agent.ReAct as ReActTest
import qualified Test.Langchain.Chain.RetrievalQASpec as RetrievalQATest
import qualified Test.Langchain.DocumentLoader.Core as DocumentLoaderTest
import qualified Test.Langchain.DocumentLoader.DirectoryLoader as DirectoryLoaderTest
import qualified Test.Langchain.Embeddings.Core as EmbeddingsTest
import qualified Test.Langchain.Error as ErrorTest
import qualified Test.Langchain.Memory.Core as MemoryTest
import qualified Test.Langchain.Memory.TokenBufferMemory as TokenBufferMemoryTest
import qualified Test.Langchain.OutputParser.Core as OutputParserTest
import qualified Test.Langchain.PreludeSpec as PreludeTest
import qualified Test.Langchain.PromptTemplate as PromptTemplateTest
import qualified Test.Langchain.Provider.Anthropic as AnthropicProviderTest
import qualified Test.Langchain.Provider.DeepSeek as DeepSeekProviderTest
import qualified Test.Langchain.Provider.Gemini as GeminiProviderTest
import qualified Test.Langchain.Provider.Ollama as OllamaProviderTest
import qualified Test.Langchain.Provider.OpenAI as OpenAIProviderTest
import qualified Test.Langchain.Retriever.Core as RetrieverTest
import qualified Test.Langchain.TextSplitter.Character as TextSplitterTest
import qualified Test.Langchain.Tool.Calculator as CalculatorToolTest
import qualified Test.Langchain.Tool.Core as ToolTest
import qualified Test.Langchain.Tool.FileSystem as FileSystemToolTest
import qualified Test.Langchain.VectorStore.Core as VectorStoreTest

main :: IO ()
main =
  defaultMain $
    testGroup
      "Langchain"
      [ ErrorTest.tests
      , PromptTemplateTest.tests
      , OutputParserTest.tests
      , TextSplitterTest.tests
      , DocumentLoaderTest.tests
      , DirectoryLoaderTest.tests
      , MemoryTest.tests
      , VectorStoreTest.tests
      , EmbeddingsTest.tests
      , RetrieverTest.tests
      , RetrievalQATest.tests
      , ToolTest.tests
      , ReActTest.tests
      , TokenBufferMemoryTest.tests
      , OllamaProviderTest.tests
      , DeepSeekProviderTest.tests
      , OpenAIProviderTest.tests
      , AnthropicProviderTest.tests
      , GeminiProviderTest.tests
      , CalculatorToolTest.tests
      , FileSystemToolTest.tests
      , PreludeTest.tests
      ]
