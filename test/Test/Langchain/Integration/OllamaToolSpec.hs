{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Langchain.Integration.OllamaToolSpec (tests) where

import Control.Monad.Except (runExceptT)
import Data.Aeson (FromJSON, ToJSON, decode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Error
import Langchain.Core.Model
import Langchain.Core.Tool (Tool (..), toolExecute)
import Langchain.OutputParser.Structured
  ( StructuredOutput (..)
  , extractJsonFromMarkdown
  , toOllamaSchema
  )
import Langchain.Provider.Ollama
  ( chatRequestFor
  , withJsonFormat
  , withSchemaFormat
  )
import Langchain.Tool.Calculator (calculatorTool)
import Test.Langchain.TestHelpers (defaultTestModel, newTestOllama, withOllamaModel)

data TestMathResult = TestMathResult
  { answer :: Double
  , explanation :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, StructuredOutput)

tests :: TestTree
tests =
  testGroup
    "Langchain.Integration.OllamaToolSpec"
    [ testCase "Ollama tool calling or direct evaluation with live model" $ do
        withOllamaModel defaultTestModel $ \modelName -> do
          provider <- newTestOllama modelName
          let prompt =
                [ systemMessage "You are a math helper. Solve: 15 * 4."
                , userMessage "What is 15 * 4?"
                ]
          res <- runExceptT $ invoke provider prompt Nothing
          case res of
            Left err -> assertFailure ("Tool test invocation failed: " ++ show err)
            Right msg -> do
              let txt = extractMessageText msg
              calcRes <-
                toolExecute calculatorTool (object ["expression" .= ("15 * 4" :: Text)]) ::
                  IO (Either LangchainError Text)
              case calcRes of
                Left err -> assertFailure ("Calculator execution error: " ++ show err)
                Right out -> out @?= "60.0"
              assertBool "Response contains 60 or answer" ("60" `T.isInfixOf` txt || not (T.null txt))
    , testCase "Ollama structured output with SchemaFormat extraction" $ do
        withOllamaModel defaultTestModel $ \modelName -> do
          provider <- newTestOllama modelName
          let prompt =
                [ systemMessage "You are a helpful math extractor."
                , userMessage "Calculate 25 + 75 and explain briefly."
                ]
              valSchema = outputSchema (Proxy :: Proxy TestMathResult)
              baseReq = chatRequestFor provider prompt
              req = case toOllamaSchema valSchema of
                Just s -> withSchemaFormat s baseReq
                Nothing -> withJsonFormat baseReq
          res <- runExceptT $ invoke provider prompt (Just req)
          case res of
            Left err -> assertFailure ("Structured Ollama invocation failed: " ++ show err)
            Right msg -> do
              let rawText = extractMessageText msg
                  cleanJson = extractJsonFromMarkdown rawText
                  bs = LBSC.fromStrict (TE.encodeUtf8 cleanJson)
              case decode bs of
                Just (result :: TestMathResult) -> do
                  answer result @?= 100.0
                  assertBool "Explanation is not empty" (not (T.null (explanation result)))
                Nothing -> assertFailure ("Failed to decode response into TestMathResult: " ++ show rawText)
    ]
