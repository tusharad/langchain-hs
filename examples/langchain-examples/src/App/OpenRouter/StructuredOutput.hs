{-# LANGUAGE OverloadedStrings #-}

module App.OpenRouter.StructuredOutput (runApp) where

import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Langchain.LLM.Internal.OpenAI as Internal
import Langchain.LLM.Internal.SchemaBuilder
import Langchain.LLM.OpenAICompatible
import System.Environment (lookupEnv)

runApp :: IO ()
runApp = do
  aKey <- T.pack . fromMaybe "api-key" <$> lookupEnv "OPENAI_API_KEY"
  let openRouter =
        mkOpenRouter
          "deepseek/deepseek-chat-v3-0324:free"
          []
          Nothing
          aKey
  let schema =
        buildSchema $
          emptyObject
            |+ ( "friends"
               , JArray
                   ( JObject
                       ( buildSchema $
                           emptyObject
                             |+ ("name", JString)
                             |+ ("age", JNumber)
                             |+ ("isAvailable", JBoolean)
                             |! "name"
                             |! "age"
                             |! "isAvailable"
                       )
                   )
               )
  let prompt =
        "I have two friends. The first is Ollama 22 years old busy saving the world,"
          <> "and the second is Alonso 23 years old and wants to hang out."
          <> "Return a list of friends in JSON format"
  let messageList = NE.singleton (Message User prompt defaultMessageData)
  let mbOpenAIParams =
        Just $
          defaultOpenAIParams
            { responseFormat =
                Just $
                  Internal.JsonSchemaFormat "SomeSchema" schema False
            }
  eRes <- chat openRouter messageList mbOpenAIParams
  case eRes of
    Left err -> putStrLn $ "Error from chat: " ++ show err
    Right (Message _ r _) -> do
      putStrLn "LLM response"
      T.putStrLn r
