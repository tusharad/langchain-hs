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
  o <- newOllama "gemma3" defaultConfig
  let msg = [userMessage inputPrompt]
  let chatReq = withStructuredOutput @Person (chatRequestFor o msg)
  res <- runLangchainTIO $ do
    invoke o msg (Just chatReq)
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right r -> T.putStrLn $ extractMessageText r
