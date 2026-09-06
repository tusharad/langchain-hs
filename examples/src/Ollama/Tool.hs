{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Ollama.Tool (runApp) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Langchain.Prelude
import Langchain.Tool.Calculator (calculatorTool)

inputPrompt :: T.Text
inputPrompt = "What is 15 * 4? Please use the calculator tool to find the answer."

runApp :: IO ()
runApp = do
  o <- newOllama "qwen3.5:2b" defaultConfig
  let promptMsgs = [userMessage inputPrompt]
      chatReq = withTools [calculatorTool @IO] (chatRequestFor o promptMsgs)

  res <- runLangchainTIO $ do
    respMsg <- invoke o promptMsgs (Just chatReq)
    case messageToolCalls respMsg of
      Nothing -> do
        liftIO $ T.putStrLn "No tool called, direct answer:"
        liftIO $ T.putStrLn $ extractMessageText respMsg
      Just [] -> do
        liftIO $ T.putStrLn "No tool called, direct answer:"
        liftIO $ T.putStrLn $ extractMessageText respMsg
      Just (tCall : _) -> do
        liftIO $ T.putStrLn $ "Tool called: " <> toolCallName tCall
        liftIO $ T.putStrLn $ "Arguments: " <> T.pack (show (toolCallArguments tCall))

        eExec <- liftIO $ toolExecute (calculatorTool @IO) (toolCallArguments tCall)
        toolResult <- case eExec of
          Left err -> pure $ "Tool execution error: " <> errorMessage err
          Right out -> pure out

        liftIO $ T.putStrLn $ "Tool execution result: " <> toolResult

        let toolMsg =
              (toolMessage toolResult)
                { messageName = Just (toolCallName tCall)
                }
            conversation = promptMsgs ++ [respMsg, toolMsg]
            followUpReq = withTools [calculatorTool @IO] (chatRequestFor o conversation)

        finalMsg <- invoke o conversation (Just followUpReq)
        liftIO $ T.putStrLn "\nFinal Assistant Answer:"
        liftIO $ T.putStrLn $ extractMessageText finalMsg

  case res of
    Left err -> T.putStrLn $ "Error: " <> errorMessage err
    Right () -> pure ()
