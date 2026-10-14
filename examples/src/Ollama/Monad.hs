{-# LANGUAGE OverloadedStrings #-}

module Ollama.Monad (runApp) where

import qualified Data.Text.IO as T
import Langchain.Prelude
import Langchain.Provider.Ollama

runApp :: IO ()
runApp = do
  o <- newOllama "gemma3" defaultConfig
  let msg = [userMessage "Write a poem about functional programming"]
  res <- runLangchainT () $ do
    let cfg =
          withOptions
            (defaultOptions {optTemperature = Just 0.7, optTopP = Just 0.9, optNumCtx = Just 100096})
            defaultOllamaConfig
    invoke o msg (Just cfg)
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right m -> T.putStrLn $ extractMessageText m
