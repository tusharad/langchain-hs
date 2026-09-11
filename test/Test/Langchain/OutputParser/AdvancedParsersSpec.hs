{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Langchain.OutputParser.AdvancedParsersSpec (tests) where

import Control.Monad.Except (runExceptT)
import Data.Aeson (FromJSON, ToJSON, Value (..))
import qualified Data.Aeson.KeyMap as KM
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Model (userMessage)
import Langchain.OutputParser.Structured
import Test.Langchain.Provider.Mock (newMockModel)

data TestPerson = TestPerson
  { personName :: Text
  , personAge :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, StructuredOutput)

instance TypeSchema TestPerson

data TestOptionalPerson = TestOptionalPerson
  { optName :: Text
  , optBio :: Maybe Text
  , optRating :: Maybe Double
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, StructuredOutput)

data TestCompany = TestCompany
  { companyName :: Text
  , companyFounder :: TestPerson
  , companyEmployees :: [TestPerson]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, StructuredOutput)

tests :: TestTree
tests =
  testGroup
    "Langchain.OutputParser.AdvancedParsersSpec"
    [ testCase "structuredInvoke extracts typed data structure from JSON output" $ do
        let mockModel = newMockModel "```json\n{\"personName\":\"Grace Hopper\",\"personAge\":85}\n```"
        res <- runExceptT $ structuredInvoke mockModel [userMessage "Who was Grace Hopper?"]
        case res of
          Left err -> assertFailure ("structuredInvoke failed: " ++ show err)
          Right (person :: TestPerson) -> do
            personName person @?= "Grace Hopper"
            personAge person @?= 85
    , testCase "optional fields are omitted from required schema list" $ do
        let s = outputSchema (Proxy :: Proxy TestOptionalPerson)
        case s of
          Object obj -> case KM.lookup "required" obj of
            Just (Array arr) -> do
              let reqs = [t | String t <- V.toList arr]
              reqs @?= ["optName"]
            _ -> assertFailure "Expected required array in schema"
          _ -> assertFailure "Expected Object schema"
    , testCase "nested records generate composite JSON schema objects" $ do
        let s = outputSchema (Proxy :: Proxy TestCompany)
        case s of
          Object obj -> case KM.lookup "properties" obj of
            Just (Object pObj) -> do
              case KM.lookup "companyFounder" pObj of
                Just (Object fObj) -> KM.lookup "type" fObj @?= Just (String "object")
                _ -> assertFailure "Expected companyFounder to be object"
              case KM.lookup "companyEmployees" pObj of
                Just (Object eObj) -> KM.lookup "type" eObj @?= Just (String "array")
                _ -> assertFailure "Expected companyEmployees to be array"
            _ -> assertFailure "Expected properties in schema"
          _ -> assertFailure "Expected Object schema"
    , testCase "toOllamaSchema and fromOllamaSchema bridge round-trip" $ do
        let s = outputSchema (Proxy :: Proxy TestCompany)
        case toOllamaSchema s of
          Nothing -> assertFailure "toOllamaSchema failed for TestCompany"
          Just ollamaS -> do
            let rt = fromOllamaSchema ollamaS
            toOllamaSchema rt @?= Just ollamaS
    ]
