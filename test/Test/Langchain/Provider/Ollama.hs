{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Provider.Ollama (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Except (runExceptT)
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Model
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
    , testCase "newOllamaWithOptions sets initial ModelOptions" $ do
        let opts = defaultOptions {optTemperature = Just 0.3, optNumCtx = Just 4096}
        p <- newOllamaWithOptions testModelName opts
        ollamaOptions p @?= Just opts
    , testCase "withTemperature, withTopP, withNumCtx modify Ollama options" $ do
        p <- newOllama testModelName
        let p' = withTemperature 0.7 $ withTopP 0.9 $ withNumCtx 2048 p
        case ollamaOptions p' of
          Nothing -> assertFailure "Expected options to be set"
          Just opts -> do
            optTemperature opts @?= Just 0.7
            optTopP opts @?= Just 0.9
            optNumCtx opts @?= Just 2048
    , testCase "chatRequestFor inherits Ollama options and keepAlive" $ do
        p <- newOllama testModelName
        let p' = withTemperature 0.5 $ withKeepAlive "10m" p
            req = chatRequestFor p' [userMessage "Hello"]
        chatKeepAlive req @?= Just "10m"
        case chatOptions req of
          Nothing -> assertFailure "Expected chatOptions in ChatRequest"
          Just opts -> optTemperature opts @?= Just 0.5
    , testCase "withTemperature and withNumCtx modify ChatRequest options" $ do
        p <- newOllama testModelName
        let req = chatRequestFor p [userMessage "Hello"]
            req' = withTemperature 0.2 $ withNumCtx 1024 req
        case chatOptions req' of
          Nothing -> assertFailure "Expected chatOptions in ChatRequest"
          Just opts -> do
            optTemperature opts @?= Just 0.2
            optNumCtx opts @?= Just 1024
    ]
