{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.OpenRouter.ToolCall (runApp) where

import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as HM
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.LLM.Gemini
import qualified OpenAI.V1.Chat.Completions as CreateChat hiding
  ( ChatCompletionChunk (..)
  , ChatCompletionObject (..)
  )
import qualified OpenAI.V1.Models as Models
import qualified OpenAI.V1.Tool as OpenAIV1 (Function (..), Tool (..))
import System.Environment

addTwoNumbers :: Int -> Int -> Int
addTwoNumbers = (+)

addTwoNumbersTool :: OpenAIV1.Tool
addTwoNumbersTool =
  OpenAIV1.Tool_Function
    OpenAIV1.Function
      { description = Just "Perform addition of given two numbers"
      , name = "addTwoNumbers"
      , parameters =
          Just . Aeson.object $
            [ "type" .= ("object" :: Text)
            , "properties"
                .= Aeson.object
                  [ "a"
                      .= Aeson.object
                        [ "type" .= ("number" :: Text)
                        , "description" .= ("first number" :: Text)
                        ]
                  , "b"
                      .= Aeson.object
                        [ "type" .= ("number" :: Text)
                        , "description" .= ("second number" :: Text)
                        ]
                  ]
            , "required" .= (["a", "b"] :: [Text])
            , "additionalProperties" .= False
            ]
      , strict = Just True
      }

runApp :: IO ()
runApp = do
  mbAKey <- lookupEnv "GEMINI_API_KEY"
  case mbAKey of
    Nothing -> putStrLn "Please provide API Key"
    Just aKey -> do
      let gemini =
            Gemini
              { apiKey = T.pack aKey
              , callbacks = []
              , baseUrl = Nothing
              }
      let messageList =
            NE.singleton
              ( Message
                  User
                  "What is 23+46? (Use tool)"
                  defaultMessageData
              )
      eRes <-
        chat
          gemini
          messageList
          ( Just $
              CreateChat._CreateChatCompletion
                { CreateChat.model = Models.Model "gemini-2.5-flash"
                , CreateChat.tools = Just [addTwoNumbersTool]
                }
          )
      case eRes of
        Left err -> putStrLn $ "Error from chat: " ++ show err
        Right r -> do
          putStrLn "LLM response"
          case toolCalls (messageData r) of
            Nothing ->
              putStrLn $
                "Message not found from chat response. "
                  <> show r
            Just toolCallList -> mapM_ executeFunction toolCallList

convertToNumber :: Value -> Maybe Int
convertToNumber (Number n) = toBoundedInteger n
convertToNumber _ = Nothing

executeFunction :: ToolCall -> IO ()
executeFunction (ToolCall _ _ ToolFunction {..}) = do
  if toolFunctionName == "addTwoNumbers"
    then do
      case HM.lookup "a" toolFunctionArguments >>= convertToNumber of
        Nothing -> putStrLn "Parameter a not found"
        Just firstNum_ -> do
          case HM.lookup "b" toolFunctionArguments >>= convertToNumber of
            Nothing -> putStrLn "Parameter b not found"
            Just secondNum_ -> do
              let firstNum = firstNum_
              let secondNum = secondNum_
              let res = addTwoNumbers firstNum secondNum
              print ("result: " :: String, res)
    else
      putStrLn "Expected function name to be addTwoNumbers"
