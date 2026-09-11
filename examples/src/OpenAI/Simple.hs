{-# LANGUAGE OverloadedStrings #-}

module OpenAI.Simple (runApp) where

import Control.Monad.Except (runExceptT)
import Data.Aeson (object, (.=))
import qualified Data.Map as Map
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as T
import Langchain.Prelude
import Langchain.PromptTemplate.Prompt
import OpenAI.Common (defaultModelName, getOpenRouterModel)

runPromptTemplateExample :: IO ()
runPromptTemplateExample = do
  let items = Map.fromList [("name", "John"), ("age", "25")]
  let inputText = "{name} is of age of {age}, he is only {age}!"
  case renderFStringTemplate items inputText of
    Left err -> T.putStrLn $ errorMessage err
    Right r -> T.putStrLn r

runApp :: IO ()
runApp = do
  runPromptTemplateExample
  let prompt = "Write a poem about functional programming"
  let splittedChars = splitTextRecursive defaultRecursiveCharacterSplitterOps prompt
  mapM_ (T.putStrLn . T.toStrict) splittedChars
  o <- getOpenRouterModel defaultModelName
  let msg = [(userMessage . T.toStrict) prompt]
  res <- runExceptT $ invoke o msg Nothing
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right m -> T.putStrLn $ extractMessageText m

  let reqWithOptions =
        object
          [ "temperature" .= (0.7 :: Double)
          , "max_tokens" .= (1024 :: Int)
          ]
  resWithOptions <- runExceptT $ invoke o msg (Just reqWithOptions)
  case resWithOptions of
    Left err -> T.putStrLn $ errorMessage err
    Right m -> T.putStrLn $ extractMessageText m

  let ques =
        [ "What is Functor in Haskell?"
        , "What is applicative in Haskell?"
        , "What is Monad in Haskell?"
        , "A really long question"
        ]
      msgs = map (\q -> [userMessage q]) ques
  batchRes <- runExceptT $ batch o (take 3 msgs) Nothing
  case batchRes of
    Left err -> T.putStrLn $ errorMessage err
    Right ms -> mapM_ (T.putStrLn . extractMessageText) ms
