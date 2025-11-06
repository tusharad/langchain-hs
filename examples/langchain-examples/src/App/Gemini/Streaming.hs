{-# LANGUAGE OverloadedStrings #-}

module App.Gemini.Streaming (runApp) where

import System.IO (hFlush, stdout)

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Langchain.LLM.Gemini
import qualified OpenAI.V1.Chat.Completions as Chat2 hiding (ChatCompletionObject (..))

import qualified OpenAI.V1.Chat.Completions as Chat hiding
  ( ChatCompletionChunk (..)
  , ChatCompletionObject (..)
  )
import qualified OpenAI.V1.Models as Models
import System.Environment

printChoice :: Chat2.ChatCompletionChunk -> IO ()
printChoice Chat2.ChatCompletionChunk {Chat2.choices = cs} = do
  let Chat2.ChunkChoice {Chat2.delta = d} = V.head cs
  case Chat2.delta_content d of
    Just c -> T.putStr c >> hFlush stdout
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
      eRes <-
        stream
          gemini
          msgList
          sh
          ( Just $
              Chat._CreateChatCompletion
                { Chat.model = Models.Model "gemini-2.5-flash"
                }
          )
      print eRes
