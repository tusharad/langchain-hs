{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : App.Gemini.Runnable
Description : Comprehensive example demonstrating LangChain's Runnable interface with Gemini API
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT

This module provides a detailed demonstration of the Runnable interface in LangChain-hs
using the Gemini API. It showcases various Runnable features including:

* Basic Runnable operations (invoke, batch, stream)
* Chain composition with RunnableSequence
* Conditional processing with RunnableBranch
* Input/output transformations with RunnableMap
* Utility wrappers (WithTimeout, Retry, Cached)

The Runnable interface is the foundation of LangChain Expression Language (LCEL) in Haskell,
enabling composition of different components into processing pipelines.
-}
module App.Gemini.Runnable
  ( runApp
  ) where

import Control.Monad (forM_)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Langchain.Error as Langchain
import Langchain.LLM.Gemini
import Langchain.Runnable.Chain
import Langchain.Runnable.Core
import qualified Langchain.Runnable.Core as Run
import Langchain.Runnable.Utils
import qualified OpenAI.V1.Chat.Completions as CreateChat hiding
  ( ChatCompletionChunk (..)
  , ChatCompletionObject (..)
  )
import qualified OpenAI.V1.Chat.Completions as OpenAIV1
import qualified OpenAI.V1.Models as Models
import System.Environment
import System.Exit

-- | Custom Runnable for text preprocessing
data TextPreprocessor = TextPreprocessor

instance Runnable TextPreprocessor where
  type RunnableInput TextPreprocessor = (T.Text, Maybe OpenAIV1.CreateChatCompletion)
  type
    RunnableOutput TextPreprocessor =
      (ChatHistory, Maybe OpenAIV1.CreateChatCompletion)

  invoke _ (input, mbParams) = do
    let preprocessed =
          "Please analyze the following text and provide insights: "
            <> input
        message = NE.singleton $ Message User preprocessed defaultMessageData
    return $ Right (message, mbParams)

-- | Custom Runnable for response formatting
data ResponseFormatter = ResponseFormatter

instance Runnable ResponseFormatter where
  type RunnableInput ResponseFormatter = Message
  type RunnableOutput ResponseFormatter = T.Text

  invoke _ (Message _ c _) = do
    let formatted = "Gemini Response:\n" <> c <> "\n" <> T.replicate 50 "-"
    return $ Right formatted

-- | Custom Runnable for sentiment analysis
data SentimentAnalyzer = SentimentAnalyzer

instance Runnable SentimentAnalyzer where
  type RunnableInput SentimentAnalyzer = (T.Text, Maybe OpenAIV1.CreateChatCompletion)
  type
    RunnableOutput SentimentAnalyzer =
      ( ChatHistory
      , Maybe OpenAIV1.CreateChatCompletion
      )

  invoke _ (input, mbParams) = do
    let prompt =
          "Analyze the sentiment of this text and respond with only 'POSITIVE', 'NEGATIVE', or 'NEUTRAL': "
            <> input
        message = NE.singleton $ Message User prompt defaultMessageData
    return $ Right (message, mbParams)

-- | Custom Runnable for keyword extraction
data KeywordExtractor = KeywordExtractor

instance Runnable KeywordExtractor where
  type RunnableInput KeywordExtractor = (T.Text, Maybe OpenAIV1.CreateChatCompletion)
  type
    RunnableOutput KeywordExtractor =
      ( ChatHistory
      , Maybe OpenAIV1.CreateChatCompletion
      )

  invoke _ (input, mbParams) = do
    let prompt =
          "Extract the top 5 keywords from this text, separated by commas: "
            <> input
        message = NE.singleton $ Message User prompt defaultMessageData
    return $ Right (message, mbParams)

{- | Any type for SentimentAnalyzer and KeywordExtractor
Workaround for the issue that RunnableBranch requires the same type for all branches
-}
data AnyType
  = First KeywordExtractor
  | Second SentimentAnalyzer
  | Default TextPreprocessor

instance Runnable AnyType where
  type RunnableInput AnyType = (T.Text, Maybe OpenAIV1.CreateChatCompletion)
  type RunnableOutput AnyType = (ChatHistory, Maybe OpenAIV1.CreateChatCompletion)

  invoke (First keywordExtractor) input = invoke keywordExtractor input
  invoke (Second sentimentAnalyzer) input = invoke sentimentAnalyzer input
  invoke (Default textPreprocessor) input = invoke textPreprocessor input

-- | Main application demonstrating various Runnable features
runApp :: IO ()
runApp = do
  mbApiKey <- lookupEnv "GEMINI_API_KEY"
  case mbApiKey of
    Nothing -> do
      putStrLn "Please provide GEMINI_API_KEY environment variable"
      exitFailure
    Just aKey -> do
      let gemini = defaultGemini {apiKey = T.pack aKey}

      putStrLn "LangChain Runnable Interface Demo with Gemini API"
      putStrLn $ "=" <> replicate 6 '='

      basicRunnableDemo gemini
      chainCompositionDemo gemini
      conditionalProcessingDemo gemini
      transformationDemo gemini
      utilityWrappersDemo gemini
      putStrLn "\nAll Runnable demos completed successfully!"

-- | Demonstrates basic Runnable operations: invoke, batch, and stream
basicRunnableDemo :: Gemini -> IO ()
basicRunnableDemo gemini = do
  putStrLn "\nDemo 1: Basic Runnable Operations"
  putStrLn $ "-" <> replicate 20 '-'

  -- Single invoke
  putStrLn "\nSingle Invoke:"
  let singleMessage = NE.singleton $ Message User "What is Haskell? Answer it in one sentence" defaultMessageData
  result <-
    invoke
      gemini
      ( singleMessage
      , Just $
          CreateChat._CreateChatCompletion
            { CreateChat.model = Models.Model "gemini-2.5-flash"
            }
      )
  case result of
    Left err -> do
      putStrLn $ Langchain.toString err
      exitFailure
    Right (Message _ response _) -> do
      putStrLn "Response received:"
      T.putStrLn $ T.take 100 response <> "..."

  putStrLn "\nBatch Processing:"
  let questions =
        [ "What is functional programming?"
        , "Explain monads briefly."
        , "What are the benefits of Haskell?"
        ]
      batchInputs =
        map
          ( \q ->
              ( NE.singleton $ Message User q defaultMessageData
              , Just $
                  CreateChat._CreateChatCompletion
                    { CreateChat.model = Models.Model "gemini-2.5-flash"
                    }
              )
          )
          questions

  batchResults <- batch gemini batchInputs
  case batchResults of
    Left err -> do
      putStrLn $ Langchain.toString err
      exitFailure
    Right responses -> do
      putStrLn $ "Processed " <> show (length responses) <> " questions:"
      forM_ (zip ([1 ..] :: [Int]) responses) $ \(i, Message _ response _) -> do
        putStrLn $ "  " <> show i <> ". " <> T.unpack (T.take 50 response) <> "..."

  -- Streaming
  putStrLn "\nStreaming Response:"
  let streamMessage = NE.singleton $ Message User "Tell me a short joke about programming." defaultMessageData
  putStr "Streaming: "
  streamResult <- Run.stream
    gemini
    ( streamMessage
    , Just $
        CreateChat._CreateChatCompletion
          { CreateChat.model = Models.Model "gemini-2.5-flash"
          }
    )
    $ \(Message _ c _) -> do
      T.putStr c
  case streamResult of
    Left err -> putStrLn $ "\nStream error: " <> Langchain.toString err
    Right _ -> putStrLn "\nStream completed"

chainCompositionDemo :: Gemini -> IO ()
chainCompositionDemo gemini = do
  putStrLn "\nDemo 2: Chain Composition with RunnableSequence"
  putStrLn $ "-" <> replicate 30 '-'

  let preprocessor = TextPreprocessor
      formatter = ResponseFormatter

  putStrLn "\nBuilding Processing Pipeline:"
  putStrLn " Text Input → TextPreprocessor → Gemini → ResponseFormatter"

  -- Method 1: Using chain function
  putStrLn "\n Method 1: Using chain function"
  let inputText = "Artificial Intelligence is transforming the world."

  result1 <-
    chain
      preprocessor
      gemini
      ( inputText
      , Just $
          CreateChat._CreateChatCompletion
            { CreateChat.model = Models.Model "gemini-2.5-flash"
            }
      )
  case result1 of
    Left err -> putStrLn $ "Chain error: " <> Langchain.toString err
    Right geminiResponse -> do
      result2 <- invoke formatter geminiResponse
      case result2 of
        Left err -> putStrLn $ "Format error: " <> Langchain.toString err
        Right formatted -> T.putStrLn formatted

  -- Method 2: Using RunnableSequence
  putStrLn "\nMethod 2: Using RunnableSequence"
  let pipeline = buildSequence preprocessor gemini
      fullPipeline = appendSequence pipeline formatter

  pipelineResult <-
    invoke
      fullPipeline
      ( inputText
      , Just $
          CreateChat._CreateChatCompletion
            { CreateChat.model = Models.Model "gemini-2.5-flash"
            }
      )
  case pipelineResult of
    Left err -> putStrLn $ "Pipeline error: " <> Langchain.toString err
    Right formatted -> T.putStrLn formatted

  -- Method 3: Using the |>> operator
  putStrLn "\nMethod 3: Using |>> operator"
  operatorResult <-
    (preprocessor |>> gemini)
      ( inputText
      , Just $
          CreateChat._CreateChatCompletion
            { CreateChat.model = Models.Model "gemini-2.5-flash"
            }
      )
  case operatorResult of
    Left err -> putStrLn $ "Operator chain error: " <> Langchain.toString err
    Right geminiResponse -> do
      formatResult <- invoke formatter geminiResponse
      case formatResult of
        Left err -> putStrLn $ "Format error: " <> Langchain.toString err
        Right formatted -> T.putStrLn formatted

-- | Demonstrates conditional processing with RunnableBranch
conditionalProcessingDemo :: Gemini -> IO ()
conditionalProcessingDemo gemini = do
  putStrLn "\nDemo 3: Conditional Processing with RunnableBranch"
  putStrLn $ "-" <> replicate 55 '-'

  let sentimentAnalyzer = SentimentAnalyzer
      keywordExtractor = KeywordExtractor

      -- Conditions for branching
      isQuestion (text, _) = "?" `T.isSuffixOf` T.strip text
      isShort (text, _) = T.length text < 50

      -- Create branched processor that uses the same output type
      -- We'll create a unified processor that handles all cases
      unifiedProcessor = TextPreprocessor -- Use TextPreprocessor for all cases
      textProcessor =
        RunnableBranch
          [ (isQuestion, Second sentimentAnalyzer)
          , (isShort, First keywordExtractor)
          ]
          (Default unifiedProcessor) -- default processor
  putStrLn "\nProcessing different types of text:"
  putStrLn "  - Questions → SentimentAnalyzer"
  putStrLn "  - Short text → KeywordExtractor"
  putStrLn "  - Default → SentimentAnalyzer"

  let testTexts =
        [ "How are you feeling today?" -- Question
        , "AI is amazing!" -- Short text
        , "The future of artificial intelligence looks very "
            <> "promising with all the recent developments." -- Long text
        ]

  forM_ (zip ([1 ..] :: [Int]) testTexts) $ \(i, text) -> do
    putStrLn $ "\nTest " <> show i <> ": " <> T.unpack text
    branchResult <-
      runBranch
        textProcessor
        ( text
        , Just $
            CreateChat._CreateChatCompletion
              { CreateChat.model = Models.Model "gemini-2.5-flash"
              }
        )
    case branchResult of
      Left err -> putStrLn $ "Branch error: " <> Langchain.toString err
      Right preprocessed -> do
        geminiResult <- invoke gemini preprocessed
        case geminiResult of
          Left err -> putStrLn $ "Gemini error: " <> Langchain.toString err
          Right (Message _ response _) -> do
            putStrLn "Result:"
            T.putStrLn $ "  " <> T.take 80 response <> "..."

-- | Demonstrates input/output transformations with RunnableMap
transformationDemo :: Gemini -> IO ()
transformationDemo gemini = do
  putStrLn "\nDemo 4: Input/Output Transformations with RunnableMap"
  putStrLn $ "-" <> replicate 40 '-'

  -- Create a RunnableMap that transforms input and output
  let inputTransform text =
        let enhanced = "Please provide a concise answer: " <> text
            message = NE.singleton $ Message User enhanced defaultMessageData
         in ( message
            , Just $
                CreateChat._CreateChatCompletion
                  { CreateChat.model = Models.Model "gemini-2.5-flash"
                  }
            )

      outputTransform (Message r c msgData) =
        Message r ("Summary: " <> T.take 100 c <> "...") msgData

      transformedGemini = RunnableMap inputTransform outputTransform gemini

  putStrLn "\nUsing RunnableMap to transform input and output:"
  putStrLn "  Input: Add instruction prefix"
  putStrLn "  Output: Create summary with emoji"

  let testQuestion = "What is machine learning?"

  putStrLn $ "\nOriginal question: " <> T.unpack testQuestion

  mapResult <- runMap transformedGemini testQuestion
  case mapResult of
    Left err -> putStrLn $ "Map error: " <> Langchain.toString err
    Right (Message _ transformed _) -> do
      putStrLn "Transformed result:"
      T.putStrLn $ "  " <> transformed

-- | Demonstrates utility wrappers like WithTimeout, Retry, and Cached
utilityWrappersDemo :: Gemini -> IO ()
utilityWrappersDemo gemini = do
  putStrLn "\nDemo 5: Utility Wrappers"
  putStrLn $ "-" <> replicate 35 '-'

  -- Demo WithTimeout
  putStrLn "\n🔹 WithTimeout Wrapper:"
  let timeoutGemini = WithTimeout gemini 30000000 -- 30 seconds timeout
      timeoutMessage =
        NE.singleton $
          Message User "Explain quantum computing." defaultMessageData

  putStrLn "Setting 30-second timeout for Gemini request..."
  timeoutResult <-
    invoke
      timeoutGemini
      ( timeoutMessage
      , Just $
          CreateChat._CreateChatCompletion
            { CreateChat.model = Models.Model "gemini-2.5-flash"
            }
      )
  case timeoutResult of
    Left err -> putStrLn $ "Timeout error: " <> Langchain.toString err
    Right (Message _ response _) -> do
      putStrLn "Response within timeout:"
      T.putStrLn $ "  " <> T.take 80 response <> "..."

  -- Demo Retry
  putStrLn "\nRetry Wrapper:"
  let retryGemini = Retry gemini 3 1000000 -- 3 retries, 1 second delay
      retryMessage =
        NE.singleton $
          Message User "What is the meaning of life?" defaultMessageData

  putStrLn "Setting up retry mechanism (3 attempts, 1s delay)..."
  retryResult <-
    invoke
      retryGemini
      ( retryMessage
      , Just $
          CreateChat._CreateChatCompletion
            { CreateChat.model = Models.Model "gemini-2.5-flash"
            }
      )
  case retryResult of
    Left err -> putStrLn $ "Retry failed: " <> Langchain.toString err
    Right (Message _ response _) -> do
      putStrLn "Response after retry attempts:"
      T.putStrLn $ "  " <> T.take 80 response <> "..."
