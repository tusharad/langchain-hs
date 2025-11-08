{-# LANGUAGE OverloadedStrings #-}

module App.OpenRouter.StructuredOutput (runApp) where

import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Langchain.LLM.Gemini
import OpenAI.V1.Chat.Completions (_CreateChatCompletion)
import qualified OpenAI.V1.Chat.Completions as CreateChat (CreateChatCompletion (..))
import qualified OpenAI.V1.Models as Models
import qualified OpenAI.V1.ResponseFormat as RF
import System.Environment (lookupEnv)

runApp :: IO ()
runApp = do
  aKey <- T.pack . fromMaybe "api-key" <$> lookupEnv "GEMINI_API_KEY"
  let gemini = defaultGemini {apiKey = aKey}
  let prompt =
        "I have two friends. The first is Ollama 22 years old busy saving the world,"
          <> "and the second is Alonso 23 years old and wants to hang out."
          <> "Return a list of friends in JSON format"
  let responseFormat =
        RF.JSON_Schema $
          RF.JSONSchema
            { RF.schema =
                Just $
                  Aeson.object
                    [ ("type", Aeson.String "array")
                    ,
                      ( "properties"
                      , Aeson.object
                          [ ("name", Aeson.object [("type", Aeson.String "string")])
                          , ("age", Aeson.object [("type", Aeson.String "integer")])
                          , ("hobby", Aeson.object [("type", Aeson.String "string")])
                          ]
                      )
                    ]
            , RF.description = Nothing
            , RF.name = ""
            , RF.strict = Nothing
            }
  let messageList = NE.singleton (Message User prompt defaultMessageData)
  eRes <-
    chat
      gemini
      messageList
      ( Just $
          _CreateChatCompletion
            { CreateChat.model = Models.Model "gemini-2.5-flash"
            , CreateChat.response_format = Just responseFormat
            }
      )
  case eRes of
    Left err -> putStrLn $ "Error from chat: " ++ show err
    Right (Message _ r _) -> do
      putStrLn "LLM response"
      T.putStrLn r
