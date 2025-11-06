{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.OpenRouter.ToolCall (runApp) where

import Data.Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as HM
import Data.Maybe (fromMaybe)
import Data.Scientific
import qualified Data.Text as T
import Langchain.LLM.OpenAICompatible
import System.Environment

addTwoNumbers :: Int -> Int -> Int
addTwoNumbers = (+)

runApp :: IO ()
runApp = do
  apiKey <- T.pack . fromMaybe "api-key" <$> lookupEnv "OPENAI_API_KEY"
  let openRouter =
        mkOpenRouter
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
  eRes <- chat openRouter messageList Nothing
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
