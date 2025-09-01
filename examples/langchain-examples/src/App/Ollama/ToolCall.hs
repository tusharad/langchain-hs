{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Ollama.ToolCall (runApp) where

import Data.Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as HM
import Data.Ollama.Chat
  ( FunctionDef (..)
  , FunctionParameters (..)
  , InputTool (..)
  )
import Data.Scientific
import Langchain.LLM.Ollama

addTwoNumbers :: Int -> Int -> Int
addTwoNumbers = (+)

runApp :: IO ()
runApp = do
  let ollamaLLM = Ollama "qwen3:0.6b" []
  let messageList =
        NE.singleton
          ( Message
              User
              "What is 23+46? (Use tool)"
              defaultMessageData
          )
      paramProp =
        HM.fromList
          [ ("a", FunctionParameters "number" Nothing Nothing Nothing)
          , ("b", FunctionParameters "number" Nothing Nothing Nothing)
          ]
      functionParams =
        FunctionParameters
          { parameterType = "object"
          , requiredParams = Just ["a", "b"]
          , parameterProperties = Just paramProp
          , additionalProperties = Just False
          }
      functionDef =
        FunctionDef
          { functionName = "addTwoNumbers"
          , functionDescription = Just "Add two numbers"
          , functionParameters = Just functionParams
          , functionStrict = Nothing
          }
      inputTool =
        InputTool
          { toolType = "function"
          , function = functionDef
          }
  let mbOllamaParams =
        Just $
          defaultOllamaParams
            { tools = Just [inputTool]
            }
  eRes <- chat ollamaLLM messageList mbOllamaParams
  case eRes of
    Left err -> putStrLn $ "Error from chat: " ++ show err
    Right r -> do
      putStrLn "LLM response"
      case toolCalls (messageData r) of
        Nothing -> putStrLn "Message not found from chat response"
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
