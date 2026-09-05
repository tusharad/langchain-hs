{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Provider.Ollama (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Except (runExceptT)
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Model
import Langchain.Core.Tool (Tool)
import Langchain.Provider.Ollama
import Langchain.Tool.Calculator (calculatorTool)

import qualified Ollama.Client as OC
import qualified Ollama.Types.Format as OFormat
import qualified Ollama.Types.Tool as OTool

testModelName :: Text
testModelName = "gemma3:latest"

tests :: TestTree
tests =
  testGroup
    "Langchain.Provider.Ollama"
    [ testCase "newOllama initializes provider with defaultConfig" $ do
        p <- newOllama testModelName defaultConfig
        ollamaModelName p @?= testModelName
    , testCase "newOllama accepts custom OllamaClientConfig" $ do
        let cfg =
              defaultConfig
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
    , testCase "invoke returns Assistant message" $ do
        p <- newOllama testModelName defaultConfig
        let input = [userMessage "What is 2 + 2? Answer with just the number."]
        res <- runExceptT $ invoke p input Nothing
        case res of
          Left err -> assertFailure $ "Expected success, got error: " ++ show err
          Right msg -> do
            messageRole msg @?= Assistant
            assertBool "Should contain 4" ("4" `T.isInfixOf` extractMessageText msg)
    , testCase "batch processes multiple inputs" $ do
        p <- newOllama testModelName defaultConfig
        let inputs = [[userMessage "What is 1 + 1?"], [userMessage "What is 2 + 2?"]]
        res <- runExceptT $ batch p inputs Nothing
        case res of
          Left err -> assertFailure $ "Expected success, got error: " ++ show err
          Right msgs -> do
            length msgs @?= 2
    , testCase "withOptions sets ModelOptions on ChatRequest" $ do
        p <- newOllama testModelName defaultConfig
        let opts = defaultOptions {optTemperature = Just 0.3, optNumCtx = Just 4096}
            req = withOptions opts (chatRequestFor p [userMessage "Hello"])
        case chatOptions req of
          Nothing -> assertFailure "Expected chatOptions in ChatRequest"
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
    , testCase "withTools on ChatRequest sets chatTools" $ do
        p <- newOllama testModelName defaultConfig
        let req = withTools [calculatorTool :: Tool IO] (chatRequestFor p [userMessage "Hello"])
        case chatTools req of
          Nothing -> assertFailure "Expected chatTools in ChatRequest"
          Just ts -> length ts @?= 1
    , testCase "chatRequestFor creates base request" $ do
        p <- newOllama testModelName defaultConfig
        let req = chatRequestFor p [userMessage "Hello"]
        chatModel req @?= ModelName testModelName
    , testCase "invoke propagates chatFormat from mbReq" $ do
        p <- newOllama testModelName defaultConfig
        let input = [userMessage "Return JSON: {\"answer\": 42}"]
            req = withJsonFormat (chatRequestFor p input)
        chatFormat req @?= Just OFormat.JsonFormat
        res <- runExceptT $ invoke p input (Just req)
        case res of
          Left err -> assertFailure $ "Expected success, got error: " ++ show err
          Right msg -> messageRole msg @?= Assistant
    ]
