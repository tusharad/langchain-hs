{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module OpenAI.StructuredOutput (runApp) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import Langchain.Prelude
import OpenAI.Common (defaultModelName, getOpenRouterModel)

data Person = Person
  { name :: T.Text
  , age :: Int
  , location :: T.Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, StructuredOutput)

inputPrompt :: T.Text
inputPrompt =
  T.unlines
    [ "For the given below information, extract information about Jesse."
    , "the 24 year old Jesse was staying in New York due to his work."
    ]

runApp :: IO ()
runApp = do
  o <- getOpenRouterModel defaultModelName
  let msg = [userMessage inputPrompt]
  let rawSchema = outputSchema (Proxy @Person)
      schema = case rawSchema of
        Object km -> Object (KM.insert "additionalProperties" (Bool False) km)
        other -> other
      chatReq =
        object
          [ "response_format"
              .= object
                [ "type" .= ("json_schema" :: T.Text)
                , "json_schema"
                    .= object
                      [ "name" .= ("person" :: T.Text)
                      , "strict" .= True
                      , "schema" .= schema
                      ]
                ]
          ]
  res <- runLangchainT () $ do
    invoke o msg (Just chatReq)
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right r -> T.putStrLn $ extractMessageText r
