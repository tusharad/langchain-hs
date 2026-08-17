module Main (main) where

import Test.Tasty

-- Unit Test Modules
import qualified Test.Langchain.Agent.AdvancedAgentsSpec as AdvancedAgentsTest
import qualified Test.Langchain.Agent.MiddlewareSpec as MiddlewareTest
import qualified Test.Langchain.Agent.ReAct as ReActTest
import qualified Test.Langchain.Cache.CacheSpec as CacheTest
import qualified Test.Langchain.Chain.AdvancedChainsSpec as AdvancedChainsTest
import qualified Test.Langchain.Chain.ChainsSpec as ChainsTest
import qualified Test.Langchain.Chain.RetrievalQASpec as RetrievalQATest
import qualified Test.Langchain.DocumentLoader.Core as DocumentLoaderTest
import qualified Test.Langchain.DocumentLoader.CsvSpec as CsvLoaderTest
import qualified Test.Langchain.DocumentLoader.DirectoryLoader as DirectoryLoaderTest
import qualified Test.Langchain.DocumentLoader.HtmlSpec as HtmlLoaderTest
import qualified Test.Langchain.DocumentLoader.JsonSpec as JsonLoaderTest
import qualified Test.Langchain.DocumentLoader.WebPageSpec as WebPageLoaderTest
import qualified Test.Langchain.Embeddings.Core as EmbeddingsTest
import qualified Test.Langchain.Error as ErrorTest
import qualified Test.Langchain.Graph.AdvancedGraphSpec as AdvancedGraphTest
import qualified Test.Langchain.Graph.CompilationSpec as GraphCompilationTest
import qualified Test.Langchain.Graph.MultiAgentPatternsSpec as MultiAgentPatternsTest
import qualified Test.Langchain.Guardrail.GuardrailSpec as GuardrailTest
import qualified Test.Langchain.MCP.McpSpec as McpTest
import qualified Test.Langchain.Memory.Core as MemoryTest
import qualified Test.Langchain.Memory.EntitySpec as EntityMemoryTest
import qualified Test.Langchain.Memory.SummarySpec as SummaryMemoryTest
import qualified Test.Langchain.Memory.TokenBufferMemory as TokenBufferMemoryTest
import qualified Test.Langchain.OutputParser.AdvancedParsersSpec as AdvancedParsersTest
import qualified Test.Langchain.OutputParser.Core as OutputParserTest
import qualified Test.Langchain.PreludeSpec as PreludeTest
import qualified Test.Langchain.PromptTemplate as PromptTemplateTest
import qualified Test.Langchain.Provider.Anthropic as AnthropicProviderTest
import qualified Test.Langchain.Provider.DeepSeek as DeepSeekProviderTest
import qualified Test.Langchain.Provider.FixturesSpec as FixturesTest
import qualified Test.Langchain.Provider.Gemini as GeminiProviderTest
import qualified Test.Langchain.Provider.Ollama as OllamaProviderTest
import qualified Test.Langchain.Provider.OllamaConversionSpec as OllamaConversionTest
import qualified Test.Langchain.Provider.OpenAI as OpenAIProviderTest
import qualified Test.Langchain.Resilience.RetrySpec as RetryTest
import qualified Test.Langchain.Retriever.AdvancedRetrieversSpec as AdvancedRetrieversTest
import qualified Test.Langchain.Retriever.Core as RetrieverTest
import qualified Test.Langchain.TextSplitter.Character as TextSplitterTest
import qualified Test.Langchain.TextSplitter.CodeSpec as CodeSplitterTest
import qualified Test.Langchain.TextSplitter.MarkdownSpec as MarkdownSplitterTest
import qualified Test.Langchain.TextSplitter.RecursiveCharacterSpec as RecursiveSplitterTest
import qualified Test.Langchain.TextSplitter.TokenSpec as TokenSplitterTest
import qualified Test.Langchain.Tool.AdvancedToolsSpec as AdvancedToolsTest
import qualified Test.Langchain.Tool.Calculator as CalculatorToolTest
import qualified Test.Langchain.Tool.Core as ToolTest
import qualified Test.Langchain.Tool.FileSystem as FileSystemToolTest
import qualified Test.Langchain.Trace.TraceSpec as TraceTest
import qualified Test.Langchain.VectorStore.Core as VectorStoreTest
import qualified Test.Langchain.VectorStore.SqliteVecSpec as SqliteVecStoreTest

-- Property Test Modules (QuickCheck Laws & Invariants)
import qualified Test.Langchain.Property.CheckpointerSpec as CheckpointerPropTest
import qualified Test.Langchain.Property.ErrorSpec as ErrorPropTest
import qualified Test.Langchain.Property.MessageSpec as MessagePropTest
import qualified Test.Langchain.Property.OutputParserSpec as OutputParserPropTest
import qualified Test.Langchain.Property.PromptTemplateSpec as PromptTemplatePropTest
import qualified Test.Langchain.Property.RunnableSpec as RunnablePropTest
import qualified Test.Langchain.Property.StateReducerSpec as StateReducerPropTest
import qualified Test.Langchain.Property.TextSplitterSpec as TextSplitterPropTest

-- Regression Test Modules
import qualified Test.Langchain.RegressionSpec as RegressionTest

-- Live Integration & E2E Test Modules (Ollama)
import qualified Test.Langchain.Integration.OllamaChatSpec as OllamaChatE2ETest
import qualified Test.Langchain.Integration.OllamaEmbeddingSpec as OllamaEmbedE2ETest
import qualified Test.Langchain.Integration.OllamaStreamSpec as OllamaStreamE2ETest
import qualified Test.Langchain.Integration.OllamaToolSpec as OllamaToolE2ETest
import qualified Test.Langchain.Integration.ReActAgentE2ESpec as ReActE2ETest
import qualified Test.Langchain.Integration.StateGraphE2ESpec as StateGraphE2ETest

main :: IO ()
main =
  defaultMain $
    testGroup
      "Langchain Test Suite"
      [ testGroup
          "Unit Tests"
          [ ErrorTest.tests
          , PromptTemplateTest.tests
          , OutputParserTest.tests
          , AdvancedParsersTest.tests
          , TextSplitterTest.tests
          , RecursiveSplitterTest.tests
          , MarkdownSplitterTest.tests
          , TokenSplitterTest.tests
          , CodeSplitterTest.tests
          , DocumentLoaderTest.tests
          , DirectoryLoaderTest.tests
          , CsvLoaderTest.tests
          , JsonLoaderTest.tests
          , HtmlLoaderTest.tests
          , WebPageLoaderTest.tests
          , MemoryTest.tests
          , SummaryMemoryTest.tests
          , EntityMemoryTest.tests
          , VectorStoreTest.tests
          , SqliteVecStoreTest.tests
          , EmbeddingsTest.tests
          , RetrieverTest.tests
          , AdvancedRetrieversTest.tests
          , RetrievalQATest.tests
          , ChainsTest.tests
          , AdvancedChainsTest.tests
          , CacheTest.tests
          , RetryTest.tests
          , ToolTest.tests
          , AdvancedToolsTest.tests
          , ReActTest.tests
          , AdvancedAgentsTest.tests
          , MultiAgentPatternsTest.tests
          , GuardrailTest.tests
          , McpTest.tests
          , TraceTest.tests
          , TokenBufferMemoryTest.tests
          , OllamaProviderTest.tests
          , OllamaConversionTest.tests
          , DeepSeekProviderTest.tests
          , OpenAIProviderTest.tests
          , AnthropicProviderTest.tests
          , GeminiProviderTest.tests
          , FixturesTest.tests
          , CalculatorToolTest.tests
          , FileSystemToolTest.tests
          , GraphCompilationTest.tests
          , AdvancedGraphTest.tests
          , MiddlewareTest.tests
          , PreludeTest.tests
          ]
      , testGroup
          "Property Tests (Laws & Invariants)"
          [ MessagePropTest.tests
          , PromptTemplatePropTest.tests
          , TextSplitterPropTest.tests
          , RunnablePropTest.tests
          , StateReducerPropTest.tests
          , CheckpointerPropTest.tests
          , ErrorPropTest.tests
          , OutputParserPropTest.tests
          ]
      , testGroup
          "Regression Tests"
          [ RegressionTest.tests
          ]
      , testGroup
          "Integration & E2E Tests (Live Ollama)"
          [ OllamaChatE2ETest.tests
          , OllamaStreamE2ETest.tests
          , OllamaToolE2ETest.tests
          , ReActE2ETest.tests
          , OllamaEmbedE2ETest.tests
          , StateGraphE2ETest.tests
          ]
      ]
