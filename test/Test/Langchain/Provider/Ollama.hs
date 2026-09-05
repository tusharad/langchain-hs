{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Provider.Ollama (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Model
import Langchain.Core.Stream (StreamEvent (..), collectEvents)
import Langchain.Provider.Ollama

import qualified Ollama.Client as OC
import qualified Ollama.Client.Config as OCC

testModelName :: Text
testModelName = "gemma3:latest"

tests :: TestTree
tests =
  testGroup
    "Langchain.Provider.Ollama"
    [ testCase "newOllama initializes provider" $ do
        p <- newOllama testModelName
        ollamaModelName p @?= testModelName
    , testCase "newOllamaWithConfig initializes provider with OllamaConfig" $ do
        let cfg =
              defaultConfig
                { configModelName = "qwen3.5:2b"
                , configBaseUrl = Just "http://custom-host:11434"
                , configTimeout = Just 120
                }
        p <- newOllamaWithConfig cfg
        ollamaModelName p @?= "qwen3.5:2b"
        OCC.configBaseUrl (OC.clientConfig (client p)) @?= "http://custom-host:11434"
        OCC.configTimeout (OC.clientConfig (client p)) @?= 120
    , testCase "newOllamaWithClient wraps existing OllamaClient" $ do
        c <- OC.defaultClient
        let p = newOllamaWithClient testModelName c
        ollamaModelName p @?= testModelName
    , testCase "invoke returns Assistant message" $ do
        p <- newOllama testModelName
        let input = [userMessage "What is 2 + 2? Answer with just the number."]
        res <- runExceptT $ invoke p input Nothing
        case res of
          Left err -> assertFailure $ "Expected success, got error: " ++ show err
          Right msg -> do
            messageRole msg @?= Assistant
            assertBool "Should contain 4" ("4" `T.isInfixOf` extractMessageText msg)
    , testCase "batch processes multiple inputs" $ do
        p <- newOllama testModelName
        let inputs = [[userMessage "What is 1 + 1?"], [userMessage "What is 2 + 2?"]]
        res <- runExceptT $ batch p inputs Nothing
        case res of
          Left err -> assertFailure $ "Expected success, got error: " ++ show err
          Right msgs -> do
            length msgs @?= 2
    , testCase "stream emits LLMStart, LLMChunk, LLMEnd" $ do
        p <- newOllama testModelName
        let input = [userMessage "Hi"]
        res <- runResourceT $ runExceptT $ collectEvents (stream p input Nothing)
        case res of
          Left err -> assertFailure $ "Expected stream success, got error: " ++ show err
          Right events -> do
            assertBool "Should emit events" (not (null events))
            case events of
              (LLMStart {} : _) -> pure ()
              _ -> assertFailure $ "Expected LLMStart event first, got: " ++ show events
    ]
