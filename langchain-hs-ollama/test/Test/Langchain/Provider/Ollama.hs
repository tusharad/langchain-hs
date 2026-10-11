{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Langchain.Provider.Ollama (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Except (runExceptT)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Langchain.Model
import Langchain.Provider.Ollama
import Langchain.Tool (Tool)
import Langchain.Tool.Binding (bindToolsConfig)
import Langchain.Tool.Calculator (calculatorTool)
import qualified Ollama.Client as OC
import qualified Ollama.Types.Format as OFormat
import qualified Ollama.Types.Message as O
import qualified Ollama.Types.Tool as OTool

testModelName :: Text
testModelName = "gemma3:latest"

-- | Skip live Ollama tests when running unit test suite
withOllamaModel :: Text -> (Text -> IO ()) -> IO ()
withOllamaModel _ _ = pure ()

data TestPerson = TestPerson
  { personName :: !Text
  , personAge :: !Int
  }
  deriving (Show, Eq, Generic, ToSchema)

tests :: TestTree
tests =
  testGroup
    "Langchain.Provider.Ollama"
    [ testCase "newOllama initializes provider with defaultConfig" $ do
        p <- newOllama testModelName defaultClientConfig
        ollamaModelName p @?= testModelName
    , testCase "newOllama accepts custom OllamaClientConfig" $ do
        let cfg =
              defaultClientConfig
                { configBaseUrl = "http://custom-host:11434"
                , configTimeout = 120
                }
        p <- newOllama "qwen3.5:2b" cfg
        ollamaModelName p @?= "qwen3.5:2b"
        configBaseUrl (OC.clientConfig (client p)) @?= "http://custom-host:11434"
        configTimeout (OC.clientConfig (client p)) @?= 120
    , testCase "newOllamaWithClient wraps existing OllamaClient" $ do
        c <- OC.defaultClient
        let p = newOllamaWithClient testModelName c
        ollamaModelName p @?= testModelName
    , testCase "defaultOllamaConfig is empty" $ do
        let cfg = defaultOllamaConfig
        ollamaTools cfg @?= Nothing
        ollamaFormat cfg @?= Nothing
        ollamaOptions cfg @?= Nothing
        ollamaKeepAlive cfg @?= Nothing
        ollamaThink cfg @?= Nothing
    , testCase "withOptions sets ModelOptions on OllamaConfig" $ do
        let opts = defaultOptions {optTemperature = Just 0.3, optNumCtx = Just 4096}
            cfg = withOptions opts defaultOllamaConfig
        case ollamaOptions cfg of
          Nothing -> assertFailure "Expected ollamaOptions in OllamaConfig"
          Just o -> do
            optTemperature o @?= Just 0.3
            optNumCtx o @?= Just 4096
    , testCase "toOllamaTool converts calculatorTool to Ollama Tool" $ do
        let cTool = calculatorTool :: Tool IO
        case toOllamaTool cTool of
          Nothing -> assertFailure "Failed to convert calculatorTool to Ollama Tool"
          Just ot -> do
            OTool.toolType ot @?= "function"
            OTool.fnName (OTool.toolFunction ot) @?= "calculator"
    , testCase "withTools sets tools on OllamaConfig" $ do
        let cfg = withTools [calculatorTool :: Tool IO] defaultOllamaConfig
        case ollamaTools cfg of
          Nothing -> assertFailure "Expected ollamaTools in OllamaConfig"
          Just ts -> length ts @?= 1
    , testCase "withJsonFormat sets JsonFormat" $ do
        let cfg = withJsonFormat defaultOllamaConfig
        ollamaFormat cfg @?= Just OFormat.JsonFormat
    , testCase "withStructuredOutput sets SchemaFormat" $ do
        let cfg = withStructuredOutput (Proxy @TestPerson) defaultOllamaConfig
        case ollamaFormat cfg of
          Just (OFormat.SchemaFormat _) -> pure ()
          _ -> assertFailure "Expected SchemaFormat in OllamaConfig"
    , testCase "withKeepAlive sets keep-alive duration" $ do
        let cfg = withKeepAlive "10m" defaultOllamaConfig
        ollamaKeepAlive cfg @?= Just "10m"
    , testCase "Semigroup (<>) merges tools and right-biases options" $ do
        let cTool = calculatorTool :: Tool IO
            cfg1 = withKeepAlive "5m" (withTools [cTool] defaultOllamaConfig)
            cfg2 = withKeepAlive "15m" (withTools [cTool] (withJsonFormat defaultOllamaConfig))
            merged = cfg1 <> cfg2
        case ollamaTools merged of
          Nothing -> assertFailure "Expected merged tools"
          Just ts -> length ts @?= 2
        ollamaKeepAlive merged @?= Just "15m"
        ollamaFormat merged @?= Just OFormat.JsonFormat
    , testCase "toOllamaRole and fromOllamaRole map correctly" $ do
        toOllamaRole User @?= O.User
        toOllamaRole Assistant @?= O.Assistant
        toOllamaRole System @?= O.System
        toOllamaRole Tool @?= O.Tool
        toOllamaRole Developer @?= O.System
        toOllamaRole Function @?= O.Tool
        fromOllamaRole O.User @?= User
        fromOllamaRole O.Assistant @?= Assistant
        fromOllamaRole O.System @?= System
        fromOllamaRole O.Tool @?= Tool
    , testCase "toOllamaMessage and fromOllamaMessage roundtrip text" $ do
        let msg = userMessage "Hello Ollama"
            oMsg = toOllamaMessage msg
            backMsg = fromOllamaMessage oMsg
        messageRole backMsg @?= User
        extractMessageText backMsg @?= "Hello Ollama"
    , testCase "ToolBinder binds tools to Ollama" $ do
        let mbCfg = bindToolsConfig @Ollama [calculatorTool :: Tool IO] Nothing
        case mbCfg of
          Nothing -> assertFailure "Expected Just OllamaConfig"
          Just cfg -> case ollamaTools cfg of
            Nothing -> assertFailure "Expected tools"
            Just ts -> length ts @?= 1
    , testCase "ToolBinder for OllamaWithTools combines tools" $ do
        p <- newOllama testModelName defaultClientConfig
        let _ = bindTools [calculatorTool :: Tool IO] p
            mbCfg = bindToolsConfig @(OllamaWithTools IO) [calculatorTool :: Tool IO] Nothing
        case mbCfg of
          Nothing -> assertFailure "Expected Just OllamaConfig"
          Just cfg -> case ollamaTools cfg of
            Nothing -> assertFailure "Expected tools"
            Just ts -> length ts @?= 1
    , testCase "invoke returns Assistant message" $ do
        withOllamaModel testModelName $ \modelName -> do
          p <- newOllama modelName defaultClientConfig
          let input = [userMessage "What is 2 + 2? Answer with just the number."]
          res <- runExceptT $ invoke p input Nothing
          case res of
            Left err -> assertFailure $ "Expected success, got error: " ++ show err
            Right msg -> do
              messageRole msg @?= Assistant
              assertBool "Should contain 4" ("4" `T.isInfixOf` extractMessageText msg)
    , testCase "batch processes multiple inputs" $ do
        withOllamaModel testModelName $ \modelName -> do
          p <- newOllama modelName defaultClientConfig
          let inputs = [[userMessage "What is 1 + 1?"], [userMessage "What is 2 + 2?"]]
          res <- runExceptT $ batch p inputs Nothing
          case res of
            Left err -> assertFailure $ "Expected success, got error: " ++ show err
            Right msgs -> do
              length msgs @?= 2
    ]
