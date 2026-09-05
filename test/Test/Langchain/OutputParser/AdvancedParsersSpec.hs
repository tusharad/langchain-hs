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
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Model (userMessage)
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Embeddings.Core (Embeddings (..))
import Langchain.OutputParser.Enum
import Langchain.OutputParser.Structured
import Langchain.OutputParser.Xml
import Langchain.Router.Semantic
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

data Sentiment = Positive | Negative | Neutral
  deriving (Show, Eq, Enum, Bounded)

data RouterMockEmbeddings = RouterMockEmbeddings

instance Embeddings RouterMockEmbeddings where
  embedDocuments _ docs = pure $ map (embedOne . TL.toStrict . pageContent) docs
  embedQuery _ q = pure $ embedOne q

embedOne :: Text -> [Float]
embedOne t
  | "billing" `T.isInfixOf` T.toLower t || "invoice" `T.isInfixOf` T.toLower t = [1.0, 0.0, 0.0]
  | "tech" `T.isInfixOf` T.toLower t || "bug" `T.isInfixOf` T.toLower t = [0.0, 1.0, 0.0]
  | otherwise = [0.0, 0.0, 1.0]

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
    , testCase "SemanticRouter routes query to best semantic route" $ do
        let emb = RouterMockEmbeddings
            routes =
              [ Route "billing" "Billing and invoices" ["Where is my invoice?", "Billing question"]
              , Route "tech_support" "Technical support and bugs" ["I found a bug in the code", "Tech issue"]
              ]
            router = newSemanticRouter emb routes 0.8
        resBilling <- runExceptT $ routeQuery router "I have a question about my invoice"
        resBilling @?= Right (Just "billing")
        resTech <- runExceptT $ routeQuery router "There is a bug in the system"
        resTech @?= Right (Just "tech_support")
    , testCase "XmlOutputParser parses XML tags into typed structure" $ do
        let xmlText = "<response><title>Haskell Guide</title><category>FP</category></response>"
            parser = newXmlOutputParser ["title", "category"] $ \elems ->
              case (lookupTag "title" elems, lookupTag "category" elems) of
                (Just t, Just c) -> Right (t, c)
                _ -> Left "Missing title or category"
            lookupTag tag es = case [xmlContent e | e <- es, xmlTag e == tag] of
              (x : _) -> Just x
              [] -> Nothing
        parseXmlOutput parser xmlText @?= Right ("Haskell Guide", "FP")
    , testCase "EnumParser parses fuzzy case-insensitive enum options" $ do
        (parseEnum "positive" :: Either Text Sentiment) @?= Right Positive
        (parseEnum "NEGATIVE" :: Either Text Sentiment) @?= Right Negative
        (parseEnum "  Neutral  " :: Either Text Sentiment) @?= Right Neutral
        (parseEnum "The sentiment is positive." :: Either Text Sentiment) @?= Right Positive
    ]
