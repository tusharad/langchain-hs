{-# LANGUAGE OverloadedStrings #-}

module Ollama.Monad (runApp) where

import qualified Data.Text.IO as T
import Langchain.Prelude

runApp :: IO ()
runApp = do
  o <- newOllama "gemma3"
  let msg = [userMessage "Write a poem about functional programming"]
  res <- runLangchainTIO $ do
    _defaultConf <- askConfig
    let chatReq = withTemperature 0.7 $ withTopP 0.9 $ withNumCtx 100096 (chatRequestFor o msg)
    invoke o msg (Just chatReq)
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right m -> T.putStrLn $ extractMessageText m
