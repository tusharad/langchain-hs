{-# LANGUAGE OverloadedStrings #-}

module Ollama.Simple (runApp) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text.IO as T
import Langchain.Prelude

runApp :: IO ()
runApp = do
  o <- newOllama "qwen3.5:9b"
  let msg = [userMessage "Write a poem about functional programming"]
  res <- runExceptT $ invoke o msg Nothing
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right m -> T.putStrLn $ extractMessageText m
