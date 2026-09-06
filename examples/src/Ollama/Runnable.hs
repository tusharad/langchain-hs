{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Ollama.Runnable (runApp) where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Langchain.Core.Runnable (Runnable (..))
import Langchain.Prelude

newtype OllamaWrapper = OllamaWrapper Ollama

instance MonadIO m => Runnable OllamaWrapper m where
  type RunnableInput OllamaWrapper = Text
  type RunnableOutput OllamaWrapper = Text
  invoke (OllamaWrapper model_) prompt = do
    res <- runExceptT $ Langchain.Prelude.invoke model_ [userMessage prompt] Nothing
    pure (extractMessageText <$> res)

runApp :: IO ()
runApp = do
  o <- OllamaWrapper <$> newOllama "qwen3.5:2b" defaultConfig
  let prompt = runLambda (\topic -> pure (Right ("Explain " <> topic <> " in two concise sentences.")))
      formatOutput = runLambda (\out -> pure (Right ("Answer:\n" <> T.strip out)))
      chain = prompt |>> runPrim o |>> formatOutput

  res <- runExceptT $ interpret chain "pure functions in Haskell"
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right ans -> T.putStrLn ans
