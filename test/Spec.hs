import qualified Test.Langchain.Agent.Core as AgentTest

-- import qualified Test.Langchain.Agent.ReactAgent as ReactAgentTest
import qualified Test.Langchain.DocumentLoader.Core as DocumentLoaderTest
import qualified Test.Langchain.DocumentLoader.DirectoryLoader as DirectoryLoaderTest
import qualified Test.Langchain.Embeddings.Core as EmbeddingsTest
import qualified Test.Langchain.LLM.Core as LLMCoreTest
import qualified Test.Langchain.LLM.Ollama as OllamaLLMTest
import qualified Test.Langchain.Memory.Core as MemoryTest
import qualified Test.Langchain.Memory.TokenBufferMemory as TokenBufferMemoryTest
import qualified Test.Langchain.OutputParser.Core as OutputParserTest
import qualified Test.Langchain.PromptTemplate as PromptTemplateTest
import qualified Test.Langchain.Retriever.Core as RetrieverTest
import qualified Test.Langchain.Runnable.Chains as RunnableChainsTest
import qualified Test.Langchain.Runnable.ConversationChains as ConverationChainsTest
import qualified Test.Langchain.Runnable.Core as RunnableTest
import qualified Test.Langchain.Runnable.Utils as RunnableUtilsTest
import qualified Test.Langchain.TextSplitter.Character as TextSplitterTest
import qualified Test.Langchain.Tool.Core as ToolTest
import qualified Test.Langchain.VectorStore.Core as VectorStoreTest
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "Langchain"
      [ LLMCoreTest.tests
      , OllamaLLMTest.tests
      , PromptTemplateTest.tests
      , OutputParserTest.tests
      , TextSplitterTest.tests
      , DocumentLoaderTest.tests
      , DirectoryLoaderTest.tests
      , MemoryTest.tests
      , VectorStoreTest.tests
      , EmbeddingsTest.tests
      , RetrieverTest.tests
      , ToolTest.tests
      , AgentTest.tests
      , -- , ReactAgentTest.tests
        RunnableTest.tests
      , RunnableUtilsTest.tests
      , RunnableChainsTest.tests
      , ConverationChainsTest.tests
      , TokenBufferMemoryTest.tests
      ]
