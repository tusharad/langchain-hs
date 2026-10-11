{-# LANGUAGE OverloadedStrings #-}

module Gemini.Simple (runApp) where

import Control.Monad.Except (runExceptT)
import qualified Data.Map as Map
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as T
import Gemini.Client (GenerationConfig (..))
import Gemini.Common (defaultGeminiModelName, getGeminiModel)
import Langchain.Prelude
import Langchain.PromptTemplate.Prompt
import Langchain.Provider.Gemini (GeminiConfig (..), defaultGeminiConfig)

runPromptTemplateExample :: IO ()
runPromptTemplateExample = do
  let items = Map.fromList [("name", "Ada Lovelace"), ("field", "Computer Science")]
  let inputText = "{name} made pioneering contributions to {field}!"
  case renderFStringTemplate items inputText of
    Left err -> T.putStrLn $ errorMessage err
    Right r -> T.putStrLn r

runApp :: IO ()
runApp = do
  T.putStrLn "--- Gemini Prompt Template ---"
  runPromptTemplateExample

  let prompt = "Explain the difference between pure and impure functions in 2 sentences"
  let splittedChars = splitTextRecursive defaultRecursiveCharacterSplitterOps prompt
  mapM_ (T.putStrLn . T.toStrict) splittedChars

  T.putStrLn "\n--- Gemini Synchronous Invocation ---"
  g <- getGeminiModel defaultGeminiModelName
  let msg = [(userMessage . T.toStrict) prompt]
  res <- runExceptT $ invoke g msg Nothing
  case res of
    Left err -> T.putStrLn $ "Error: " <> errorMessage err
    Right m -> T.putStrLn $ extractMessageText m

  T.putStrLn "\n--- Gemini With System Instruction & Generation Config ---"
  let customCfg =
        defaultGeminiConfig
          { configSystemInstruction = Just "You are a concise Haskell tutor."
          , configGenerationConfig =
              Just
                GenerationConfig
                  { maxOutputTokens = Just 128
                  , seed = Just 42
                  , stopSequences = Nothing
                  , thinkingLevel = Nothing
                  , thinkingSummaries = Nothing
                  , toolChoice = Nothing
                  }
          }
  resConfig <- runExceptT $ invoke g msg (Just customCfg)
  case resConfig of
    Left err -> T.putStrLn $ "Error: " <> errorMessage err
    Right m -> T.putStrLn $ extractMessageText m

  T.putStrLn "\n--- Gemini Batch Processing ---"
  let questions =
        [ "What is a Functor in Haskell in one sentence?"
        , "What is a Monad in Haskell in one sentence?"
        ]
      msgs = map (\q -> [userMessage q]) questions
  batchRes <- runExceptT $ batch g msgs Nothing
  case batchRes of
    Left err -> T.putStrLn $ "Error: " <> errorMessage err
    Right ms -> mapM_ (\m -> T.putStrLn $ "- " <> extractMessageText m) ms
