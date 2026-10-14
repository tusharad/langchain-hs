{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty

-- Unit Test Modules
import qualified Test.Langchain.Agent.AdvancedAgentsSpec as AdvancedAgentsTest
import qualified Test.Langchain.Agent.ReAct as ReActTest
import qualified Test.Langchain.Cache.CacheSpec as CacheTest
import qualified Test.Langchain.Callback.CallbackManagerSpec as CallbackTest
import qualified Test.Langchain.Chain.RetrievalQASpec as RetrievalQATest
import qualified Test.Langchain.DocumentLoader.Core as DocumentLoaderTest
import qualified Test.Langchain.DocumentLoader.DirectoryLoader as DirectoryLoaderTest
import qualified Test.Langchain.Error as ErrorTest
import qualified Test.Langchain.Graph.CompilationSpec as GraphCompilationTest
import qualified Test.Langchain.Guardrail.GuardrailSpec as GuardrailTest
import qualified Test.Langchain.Memory.Core as MemoryTest
import qualified Test.Langchain.Memory.SummarySpec as SummaryMemoryTest
import qualified Test.Langchain.Memory.TokenBufferMemory as TokenBufferMemoryTest
import qualified Test.Langchain.ObservabilitySpec as ObservabilityTest
import qualified Test.Langchain.OutputParser.AdvancedParsersSpec as AdvancedParsersTest
import qualified Test.Langchain.OutputParser.Core as OutputParserTest
import qualified Test.Langchain.PromptTemplate.PromptSpec as PromptTemplateTest
import qualified Test.Langchain.Resilience.CircuitBreakerSpec as CircuitBreakerTest
import qualified Test.Langchain.Resilience.RetrySpec as RetryTest
import qualified Test.Langchain.Retriever.BM25Spec as BM25Test
import qualified Test.Langchain.Retriever.Core as RetrieverTest
import qualified Test.Langchain.Retriever.HybridSpec as HybridRetrieverTest
import qualified Test.Langchain.TextSplitter.Character as TextSplitterTest
import qualified Test.Langchain.TextSplitter.CodeSpec as CodeSplitterTest
import qualified Test.Langchain.TextSplitter.MarkdownSpec as MarkdownSplitterTest
import qualified Test.Langchain.TextSplitter.RecursiveCharacterSpec as RecursiveSplitterTest
import qualified Test.Langchain.TextSplitter.TokenSpec as TokenSplitterTest
import qualified Test.Langchain.Tool.AdvancedToolsSpec as AdvancedToolsTest
import qualified Test.Langchain.Tool.FileSystem as FileSystemToolTest
import qualified Test.Langchain.Tool.ProcessSpec as ProcessToolTest
import qualified Test.Langchain.Tool.SecuritySpec as SecurityToolTest
import qualified Test.Langchain.Tool.Shell as ShellToolTest
import qualified Test.Langchain.VectorStore.Core as VectorStoreTest

-- Property Test Modules (QuickCheck Laws & Invariants)
import qualified Test.Langchain.Property.CheckpointerSpec as CheckpointerPropTest
import qualified Test.Langchain.Property.ErrorSpec as ErrorPropTest
import qualified Test.Langchain.Property.MessageSpec as MessagePropTest
import qualified Test.Langchain.Property.PromptTemplateSpec as PromptTemplatePropTest
import qualified Test.Langchain.Property.RunnableSpec as RunnablePropTest
import qualified Test.Langchain.Property.TextSplitterSpec as TextSplitterPropTest

-- Regression Test Module
import qualified Test.Langchain.RegressionSpec as RegressionTest

main :: IO ()
main =
  defaultMain $
    testGroup
      "Langchain Test Suite"
      [ testGroup
          "Unit Tests"
          [ PromptTemplateTest.tests
          , OutputParserTest.tests
          , AdvancedParsersTest.tests
          , TextSplitterTest.tests
          , RecursiveSplitterTest.tests
          , MarkdownSplitterTest.tests
          , TokenSplitterTest.tests
          , CodeSplitterTest.tests
          , DocumentLoaderTest.tests
          , DirectoryLoaderTest.tests
          , MemoryTest.tests
          , SummaryMemoryTest.tests
          , VectorStoreTest.tests
          , ErrorTest.tests
          , RetrieverTest.tests
          , BM25Test.tests
          , HybridRetrieverTest.tests
          , RetrievalQATest.tests
          , CacheTest.tests
          , RetryTest.tests
          , CircuitBreakerTest.tests
          , AdvancedToolsTest.tests
          , ReActTest.tests
          , AdvancedAgentsTest.tests
          , GuardrailTest.tests
          , ObservabilityTest.tests
          , CallbackTest.tests
          , TokenBufferMemoryTest.tests
          , FileSystemToolTest.tests
          , ProcessToolTest.tests
          , SecurityToolTest.tests
          , ShellToolTest.tests
          , GraphCompilationTest.tests
          ]
      , testGroup
          "Property Tests (Laws & Invariants)"
          [ MessagePropTest.tests
          , PromptTemplatePropTest.tests
          , TextSplitterPropTest.tests
          , RunnablePropTest.tests
          , CheckpointerPropTest.tests
          , ErrorPropTest.tests
          ]
      , testGroup
          "Regression Tests"
          [ RegressionTest.tests
          ]
      ]
