{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Langchain.OutputParser.AdvancedParsersSpec (tests) where

import Control.Monad.Except (runExceptT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Model (newMockModel, userMessage)
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Embeddings.Core (Embeddings (..))
import Langchain.OutputParser.Enum
import Langchain.OutputParser.Structured
import Langchain.OutputParser.Xml
import Langchain.Router.Semantic

data TestPerson = TestPerson
  { personName :: Text
  , personAge :: Int
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
