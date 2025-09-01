{-# LANGUAGE OverloadedStrings #-}

module App.Ollama.StructuredOutput (runApp) where

import qualified Data.List.NonEmpty as NE
import Data.Ollama.Chat
  ( Format (..)
  )
import Data.Ollama.Common.SchemaBuilder
import qualified Data.Text.IO as T
import Langchain.LLM.Ollama

runApp :: IO ()
runApp = do
  let ollamaLLM = Ollama "qwen3:0.6b" []
  let schema =
        buildSchema $
          emptyObject
            |+ ( "friends"
               , JArray
                  ( JObject
                      ( buildSchema $
                          emptyObject
                            |+ ("name", JString)
                            |+ ("age", JNumber)
                            |+ ("isAvailable", JBoolean)
                            |! "name"
                            |! "age"
                            |! "isAvailable"
                      )
                  )
               )
  let prompt =
        "I have two friends. The first is Ollama 22 years old busy saving the world,"
          <> "and the second is Alonso 23 years old and wants to hang out."
          <> "Return a list of friends in JSON format"
  let messageList = NE.singleton (Message User prompt defaultMessageData)

  let mbOllamaParams =
        Just $
          defaultOllamaParams
            { format = Just $ SchemaFormat schema
            }
  eRes <- chat ollamaLLM messageList mbOllamaParams
  case eRes of
    Left err -> putStrLn $ "Error from chat: " ++ show err
    Right (Message _ r _) -> do
      putStrLn "LLM response"
      T.putStrLn r
