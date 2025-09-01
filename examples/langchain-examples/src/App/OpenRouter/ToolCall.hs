{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.OpenRouter.ToolCall (runApp) where

import Data.Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as HM
import Data.Maybe (fromMaybe)
import Data.Scientific
import qualified Data.Text as T
import qualified Langchain.LLM.Internal.OpenAI as OpenAIInternal
import Langchain.LLM.OpenAICompatible
import System.Environment

addTwoNumbers :: Int -> Int -> Int
addTwoNumbers = (+)

runApp :: IO ()
runApp = do
  apiKey <- T.pack . fromMaybe "api-key" <$> lookupEnv "OPENAI_API_KEY"
  let openRouter =
        mkOpenRouter
          "deepseek/deepseek-chat-v3-0324:free"
          []
          Nothing
          apiKey
  let messageList =
        NE.singleton
          ( Message
              User
              "What is 23+46? (Use tool)"
              defaultMessageData
          )
      paramProp =
        HM.fromList
          [
            ( "a"
            , OpenAIInternal.FunctionParameters "number" Nothing Nothing Nothing
            )
          ,
            ( "b"
            , OpenAIInternal.FunctionParameters "number" Nothing Nothing Nothing
            )
          ]
      functionParams =
        OpenAIInternal.FunctionParameters
          { parameterType = "object"
          , requiredParams = Just ["a", "b"]
          , parameterProperties = Just paramProp
          , additionalProperties = Just False
          }
      functionDef =
        OpenAIInternal.FunctionDef
          { functionName = "addTwoNumbers"
          , functionDescription = Just "Add two numbers"
          , functionParameters = Just functionParams
          , functionStrict = Nothing
          }
      inputTool =
        OpenAIInternal.InputTool
          { toolType = "function"
          , function = functionDef
          }
  let mbOpenAIParams =
        Just $
          defaultOpenAIParams
            { tools = Just [inputTool]
            }
  eRes <- chat openRouter messageList mbOpenAIParams
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
