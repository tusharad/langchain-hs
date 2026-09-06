{-# LANGUAGE OverloadedStrings #-}

module Ollama.Memory (runApp) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Langchain.Prelude

runApp :: IO ()
runApp = do
  o <- newOllama "gemma3" defaultConfig
  mem <- newWindowBufferMemory 5 [systemMessage "You are a helpful assistant."]
  res <- runExceptT $ do
    chat_ o mem "Hi, my name is Alice."
    chat_ o mem "What is my name?"
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right _ -> pure ()

chat_ :: Ollama -> WindowBufferMemory -> Text -> ExceptT LangchainError IO ()
chat_ model mem prompt = do
  addUserMessage mem prompt
  history <- messages mem
  resp <- invoke model history Nothing
  let answer = extractMessageText resp
  addAiMessage mem answer
  liftIO $ do
    T.putStrLn $ "User: " <> prompt
    T.putStrLn $ "AI: " <> answer
