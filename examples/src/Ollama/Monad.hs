{-# LANGUAGE OverloadedStrings #-}

module Ollama.Monad (runApp) where

import qualified Data.Text.IO as T
import Langchain.Prelude

runApp :: IO ()
runApp = do
  o <- newOllama "gemma3" defaultConfig
  let msg = [userMessage "Write a poem about functional programming"]
  res <- runLangchainTIO $ do
    _defaultConf <- askConfig
    let chatReq =
          withOptions
            (defaultOptions {optTemperature = Just 0.7, optTopP = Just 0.9, optNumCtx = Just 100096})
            (chatRequestFor o msg)
    invoke o msg (Just chatReq)
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right m -> T.putStrLn $ extractMessageText m
