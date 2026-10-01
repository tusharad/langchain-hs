{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module OpenAI.StructuredOutput (runApp) where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import Langchain.Prelude
import Langchain.Provider.OpenAI (OpenAIOptions (OpenAIOptions), ToSchema)
import OpenAI.Common (defaultModelName, getOpenRouterModel)

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
  openai <- getOpenRouterModel defaultModelName
  let msgs = [userMessage inputPrompt]
      opts = withStructuredOutput @Person (OpenAIOptions (object []))
  res <- runLangchainT () $ invoke openai msgs (Just opts)
  T.putStrLn $ either errorMessage extractMessageText res
