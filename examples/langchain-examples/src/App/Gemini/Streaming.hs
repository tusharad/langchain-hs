{-# LANGUAGE OverloadedStrings #-}

module App.Gemini.Streaming (runApp) where

import System.IO (hFlush, hPutStrLn, stderr, stdout)

import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Langchain.LLM.Gemini
import qualified OpenAI.V1.Chat.Completions as Chat hiding (ChatCompletionObject (..))
import System.Environment

printChoice Chat.ChatCompletionChunk {Chat.choices = cs} = do
  let Chat.ChunkChoice {Chat.delta = d} = V.head cs
  case Chat.delta_content d of
    Just content -> T.putStr content >> hFlush stdout
    Nothing -> pure ()

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
              { onToken = printChoice
              , onComplete = putStrLn "completed"
              }
      eRes <- stream gemini msgList sh Nothing
      print eRes
