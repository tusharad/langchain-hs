{-# LANGUAGE OverloadedStrings #-}

module Ollama.Simple (runApp) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text.IO as T
import Langchain.Prelude

runApp :: IO ()
runApp = do
  o <- newOllama "gemma3"
  let msg = [userMessage "Write a poem about functional programming"]
  res <- runExceptT $ invoke o msg Nothing
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right m -> T.putStrLn $ extractMessageText m

  -- Example showcasing passing chatOptions (temperature, topP, numCtx) on the model:
  let oWithOptions = withTemperature 0.7 $ withTopP 0.9 $ withNumCtx 4096 o
  resWithOptions <- runExceptT $ invoke oWithOptions msg Nothing
  case resWithOptions of
    Left err -> T.putStrLn $ errorMessage err
    Right m -> T.putStrLn $ extractMessageText m

  -- Example showcasing passing chatOptions per request using chatRequestFor:
  let req = withTemperature 0.2 $ withNumCtx 2048 (chatRequestFor o msg)
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
