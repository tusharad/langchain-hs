{-# LANGUAGE OverloadedStrings #-}

module Ollama.Simple (runApp) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text.IO as T
import Langchain.Prelude

runApp :: IO ()
runApp = do
  o <- newOllama "gemma3" defaultConfig
  let msg = [userMessage "Write a poem about functional programming"]
  res <- runExceptT $ invoke o msg Nothing
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right m -> T.putStrLn $ extractMessageText m

  -- Example showcasing passing chatOptions (temperature, topP, numCtx) per request using withOptions and defaultOptions:
  let reqWithOptions =
        withOptions
          (defaultOptions {optTemperature = Just 0.7, optTopP = Just 0.9, optNumCtx = Just 4096})
          (chatRequestFor o msg)
  resWithOptions <- runExceptT $ invoke o msg (Just reqWithOptions)
  case resWithOptions of
    Left err -> T.putStrLn $ errorMessage err
    Right m -> T.putStrLn $ extractMessageText m

  -- Example showcasing custom chatOptions per request:
  let req =
        withOptions
          (defaultOptions {optTemperature = Just 0.2, optNumCtx = Just 2048})
          (chatRequestFor o msg)
  resWithReq <- runExceptT $ invoke o msg (Just req)
  case resWithReq of
    Left err -> T.putStrLn $ errorMessage err
    Right m -> T.putStrLn $ extractMessageText m

  -- another example showcasing batch usage.
  let ques =
        [ "What is Functor in Haskell?"
        , "What is applicative in Haskell?"
        , "What is Monad in Haskell?"
        , "A really long question"
        ]
      msgs = map (\q -> [userMessage q]) ques
  batchRes <- runExceptT $ batch o (take 3 msgs) Nothing
  case batchRes of
    Left err -> T.putStrLn $ errorMessage err
    Right ms -> mapM_ (T.putStrLn . extractMessageText) ms
