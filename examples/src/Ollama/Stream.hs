{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Ollama.Stream (runApp) where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.Text.IO as T
import Langchain.Prelude
import System.IO (hFlush, stdout)

runApp :: IO ()
runApp = do
  o <- newOllama "qwen3.5:9b"
  let msgs = [userMessage "Give me the list of last 5 pres of US"]
  res <- runExceptT $ runConduit $ stream o msgs Nothing .| CL.mapM_ onStreamEvent
  case res of
    Left err -> T.putStrLn $ "\nError: " <> errorMessage err
    Right () -> T.putStrLn "\n--- Stream Finished ---"
  where
    onStreamEvent = \case
      LLMChunk _ chunk _ -> liftIO $ do
        T.putStr chunk
        hFlush stdout
      _ -> pure ()
