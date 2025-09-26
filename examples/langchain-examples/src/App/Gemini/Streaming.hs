{-# LANGUAGE OverloadedStrings #-}

module App.Gemini.Streaming (runApp) where

import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Langchain.LLM.Gemini
import qualified Langchain.LLM.Internal.OpenAI as OpenAI
import System.Environment

openAIChunkToText :: OpenAI.ChatCompletionChunk -> T.Text
openAIChunkToText completionChunk = do
  fromMaybe ""
    . OpenAI.contentForDelta
    . OpenAI.delta
    . fromMaybe emptyChoice
    . listToMaybe
    $ OpenAI.chunkChoices completionChunk
  where
    emptyChoice = OpenAI.ChunkChoice (OpenAI.Delta Nothing) Nothing

runApp :: IO ()
runApp = do
  mbAKey <- lookupEnv "GEMINI_API_KEY"
  case mbAKey of
    Nothing -> putStrLn "Please provide api key"
    Just aKey -> do
      let gemini =
            Gemini
              { apiKey = T.pack aKey
              , geminiModelName = "gemini-2.5-flash"
              , callbacks = []
              , baseUrl = Nothing
              }
      let msgList =
            NE.fromList
              [ Message
                  User
                  "What is the meaning of life?"
                  defaultMessageData
              ]
      let sh =
            StreamHandler
              { onToken = T.putStr . openAIChunkToText
              , onComplete = putStrLn "completed"
              }
      eRes <- stream gemini msgList sh Nothing
      print eRes
