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

  -- another example showcasing batch usage.
  let ques = ["What is Functor in Haskell?", "What is applicative in Haskell?", "What is Monad in Haskell?", "A really long question"]
      msgs = map (\q -> [userMessage q]) ques
  batchRes <- runExceptT $ batch o (take 3 msgs) Nothing
  case batchRes of
    Left err -> T.putStrLn $ errorMessage err
    Right ms -> mapM_ (T.putStrLn . extractMessageText) ms
