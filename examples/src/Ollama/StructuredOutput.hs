{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Ollama.StructuredOutput (runApp) where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics
import Langchain.Prelude
import Langchain.Provider.Ollama

data Person = Person
  { name :: T.Text
  , age :: Int
  , location :: T.Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToSchema)

inputPrompt :: T.Text
inputPrompt =
  T.unlines
    [ "For the given below information, extract information about Jesse."
    , "the 24 year old Jesse was staying in New York due to his work."
    ]

runApp :: IO ()
runApp = do
  ollama <- newOllama "gemma3" defaultConfig
  let msgs = [userMessage inputPrompt]
      chatReq = chatRequestFor ollama msgs
      opts = withStructuredOutput @Person (OllamaOptions chatReq)
  res <- runLangchainT () $ invoke ollama msgs (Just opts)
  T.putStrLn $ either errorMessage extractMessageText res
