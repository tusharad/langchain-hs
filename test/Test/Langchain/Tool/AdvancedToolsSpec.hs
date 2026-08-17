{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Tool.AdvancedToolsSpec (tests) where

import Control.Concurrent.Async (wait)
import Control.Monad.Except (runExceptT)
import Data.Aeson (FromJSON, ToJSON, Value (..), object)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Tool.Async
import Langchain.Tool.Core (createTool)
import Langchain.Tool.GenericSchema

data SearchArgs = SearchArgs
  { queryTerm :: Text
  , maxResults :: Int
  , filterCategory :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, DeriveToolSchema)

tests :: TestTree
tests =
  testGroup
    "Langchain.Tool.AdvancedToolsSpec"
    [ testCase "deriveToolSchema generates valid JSON Schema object with properties" $ do
        let schemaVal = deriveToolSchema (Proxy :: Proxy SearchArgs)
        case schemaVal of
          Object o -> assertBool "Schema contains type or properties" (not $ null o)
          _ -> assertFailure "Expected Object schema"
    , testCase "executeToolAsync runs tool in background thread" $ do
        let sampleTool =
              createTool
                "async_sample"
                "Async test"
                (object [])
                (\_ -> pure $ Right "Completed async")
        asyncHandle <- executeToolAsync sampleTool (object [])
        res <- wait asyncHandle
        res @?= Right "Completed async"
    , testCase "executeToolBatchConcurrently runs multiple tool calls concurrently" $ do
        let sampleTool =
              createTool
                "batch_sample"
                "Batch test"
                (object [])
                (\_ -> pure $ Right "Batch OK")
        res <- runExceptT $ executeToolBatchConcurrently [(sampleTool, object []), (sampleTool, object [])]
        res @?= Right ["Batch OK", "Batch OK"]
    ]
