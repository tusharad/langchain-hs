{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Gemini.Stream (runApp) where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.Text.IO as T
import Gemini.Common (defaultGeminiModelName, getGeminiModel)
import Langchain.Prelude
import System.IO (hFlush, stdout)

runApp :: IO ()
runApp = do
  T.putStrLn "--- Gemini Conduit Token Streaming ---"
  g <- getGeminiModel defaultGeminiModelName
  let msgs = [userMessage "Explain why Haskell has referential transparency in 2 short paragraphs"]
  res <- runResourceT $ runExceptT $ runConduit $ stream g msgs Nothing .| CL.mapM_ onStreamEvent
  case res of
    Left err -> T.putStrLn $ "\nError: " <> errorMessage err
    Right () -> T.putStrLn "\n--- Stream Finished ---"
  where
    onStreamEvent = \case
      LLMChunk _ chunk _ -> liftIO $ do
        T.putStr chunk
        hFlush stdout
      _ -> pure ()
