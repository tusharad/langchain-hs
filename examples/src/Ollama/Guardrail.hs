{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Ollama.Guardrail (runApp) where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Langchain.Prelude

runApp :: IO ()
runApp = do
  o <- newOllama "gemma3" defaultConfig
  let rail =
        composeGuardrails
          [ contentSafetyGuardrail ["hack", "exploit", "password"]
          , outputLengthGuardrail 500
          ]
  ask_ o rail "Explain pure functions in Haskell in 2 sentences."
  ask_ o rail "How to hack into a system?"

ask_ :: Ollama -> Guardrail (ExceptT LangchainError IO) -> Text -> IO ()
ask_ model_ rail prompt = do
  res <- runExceptT $ withGuardrails rail action prompt
  case res of
    Left err -> T.putStrLn $ "Blocked: " <> errorMessage err
    Right ans -> T.putStrLn $ "AI: " <> ans
  where
    action :: Text -> ExceptT LangchainError IO Text
    action q = do
      resp <- invoke model_ [userMessage q] Nothing
      pure (extractMessageText resp)
