{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.DocumentTransformer.HeaderInjectorSpec (tests) where

import Data.Aeson (Value (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.DocumentLoader.Core (Document (..))
import Langchain.DocumentTransformer.HeaderInjector

tests :: TestTree
tests =
  testGroup
    "Langchain.DocumentTransformer.HeaderInjector"
    [ testCase "injectChunkHeader prepends formatted metadata header" $ do
        let doc =
              Document
                { pageContent = "This is the body content of chapter 1."
                , metadata =
                    Map.fromList
                      [ ("title", String "Architecture Guide")
                      , ("section", String "Core Pipeline")
                      ]
                }
            enriched = injectChunkHeader ["title", "section"] doc
            content = TL.toStrict (pageContent enriched)
        assertBool "Header prefix present" (T.isPrefixOf "=== [Header: " content)
        assertBool "Title included" (T.isInfixOf "title: Architecture Guide" content)
        assertBool "Section included" (T.isInfixOf "section: Core Pipeline" content)
        assertBool "Body preserved" (T.isSuffixOf "This is the body content of chapter 1." content)
    ]
