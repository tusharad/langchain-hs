{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Pure.PurePipelineSpec (tests) where

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Identity (Identity (..), runIdentity)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Error
import Langchain.PromptTemplate

-- | A purely pure chain transformation without IO
pureTransform :: Text -> Text
pureTransform txt = "TRANSFORMED: " <> T.toUpper txt

-- | Pure pipeline computation using Identity and Except
runPurePipeline :: Text -> Either LangchainError Text
runPurePipeline input =
  let rendered = renderPrompt (PromptTemplate "Hello {name}!") (Map.singleton "name" input)
   in case rendered of
        Left err -> Left err
        Right promptText -> Right (pureTransform promptText)

tests :: TestTree
tests =
  testGroup
    "Langchain.Pure.PurePipelineSpec (Zero IO)"
    [ testCase "Pure prompt rendering executes with zero IO" $ do
        let rendered = renderPrompt (PromptTemplate "Welcome {user}") (Map.singleton "user" "Alice")
        rendered @?= Right "Welcome Alice"
    , testCase "Pure pipeline transformation executes cleanly" $ do
        let res = runPurePipeline "Bob"
        res @?= Right "TRANSFORMED: HELLO BOB!"
    , testCase "Pure missing variable returns Left without side effects" $ do
        let res = renderPrompt (PromptTemplate "Missing {var}") Map.empty
        case res of
          Left _ -> pure ()
          Right _ -> assertFailure "Expected missing variable error in pure context"
    ]
