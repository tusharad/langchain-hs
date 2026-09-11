{-# LANGUAGE OverloadedStrings #-}

module OpenAI.Memory (runApp) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Langchain.Prelude
import OpenAI.Common (defaultModelName, getOpenRouterModel)

runApp :: IO ()
runApp = do
  o <- getOpenRouterModel defaultModelName
  mem <- newWindowBufferMemory 5 [systemMessage "You are a helpful assistant."]
  res <- runExceptT $ do
    chat_ o mem "Hi, my name is Alice."
    chat_ o mem "What is my name?"
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right _ -> pure ()

chat_ :: OpenAI -> WindowBufferMemory -> Text -> ExceptT LangchainError IO ()
chat_ model_ mem prompt = do
  addUserMessage mem prompt
  history <- messages mem
  resp <- invoke model_ history Nothing
  let answer = extractMessageText resp
  addAiMessage mem answer
  liftIO $ do
    T.putStrLn $ "User: " <> prompt
    T.putStrLn $ "AI: " <> answer
