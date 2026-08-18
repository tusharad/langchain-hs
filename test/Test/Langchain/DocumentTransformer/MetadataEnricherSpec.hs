{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.DocumentTransformer.MetadataEnricherSpec (tests) where

import Data.Aeson (toJSON)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as TL
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.DocumentLoader.Core (Document (..))
import Langchain.DocumentTransformer.MetadataEnricher

tests :: TestTree
tests =
  testGroup
    "Langchain.DocumentTransformer.MetadataEnricherSpec"
    [ testCase "enrichDocumentMetadata calculates word, char, and token statistics" $ do
        let doc =
              Document
                { pageContent = "Haskell is a purely functional programming language."
                , metadata = Map.empty
                }
            enriched = enrichDocumentMetadata doc
        Map.member "word_count" (metadata enriched) @?= True
        Map.member "char_count" (metadata enriched) @?= True
        Map.member "estimated_tokens" (metadata enriched) @?= True
        metadata enriched Map.!? "word_count" @?= Just (toJSON (7 :: Int))
    ]
