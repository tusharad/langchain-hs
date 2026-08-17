{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.TextSplitters.TextSplitter (tests) where

import qualified Data.Map.Strict as Map
import Data.Aeson (Value (String))
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.TextSplitters.RecursiveCharacterTextSplitter
import Langchain.TextSplitters.TextSplitter
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Langchain.TextSplitters.TextSplitter Tests"
    [ testCase "createDocuments splits texts" $ do
        let docs =
              createDocuments
                defaultCreateDocumentsOps
                wordSplitterOps
                ["foo bar", "baz"]
                [mempty, mempty]
        docs
          @?= [ Document "foo" mempty
              , Document "bar" mempty
              , Document "baz" mempty
              ]
    , testCase "createDocuments preserves metadata" $ do
        let source1 = Map.fromList [("source", String "1")]
            source2 = Map.fromList [("source", String "2")]
            docs =
              createDocuments
                defaultCreateDocumentsOps
                wordSplitterOps
                ["foo bar", "baz"]
                [source1, source2]
        docs
          @?= [ Document "foo" source1
              , Document "bar" source1
              , Document "baz" source2
              ]
    , testCase "createDocuments pads missing metadata" $ do
        let source1 = Map.fromList [("source", String "1")]
            docs =
              createDocuments
                defaultCreateDocumentsOps
                wordSplitterOps
                ["foo bar", "baz"]
                [source1]
        docs
          @?= [ Document "foo" source1
              , Document "bar" source1
              , Document "baz" mempty
              ]
    , testCase "createDocuments can add start index" $ do
        let docs =
              createDocuments
                defaultCreateDocumentsOps {addStartIndex = True}
                overlappingWordSplitterOps
                ["w1 w1 w1 w1 w1 w1 w1 w1 w1"]
                [mempty]
        docs
          @?= [ Document "w1 w1" (Map.fromList [("start_index", String "0")])
              , Document "w1 w1" (Map.fromList [("start_index", String "6")])
              , Document "w1 w1" (Map.fromList [("start_index", String "12")])
              , Document "w1 w1" (Map.fromList [("start_index", String "18")])
              , Document "w1" (Map.fromList [("start_index", String "24")])
              ]
    , testCase "createDocuments start index respects overlap" $ do
        let docs =
              createDocuments
                defaultCreateDocumentsOps {addStartIndex = True}
                overlapSplitterOps
                ["foo bar baz 123"]
                [mempty]
        docs
          @?= [ Document "foo bar" (Map.fromList [("start_index", String "0")])
              , Document "bar baz" (Map.fromList [("start_index", String "4")])
              , Document "baz 123" (Map.fromList [("start_index", String "8")])
              ]
    , testCase "splitDocuments preserves document metadata" $ do
        let source1 = Map.fromList [("source", String "1")]
            source2 = Map.fromList [("source", String "2")]
            docs =
              splitDocuments
                defaultCreateDocumentsOps
                characterSplitterOps
                [ Document "foo" source1
                , Document "bar" source2
                , Document "baz" source1
                ]
        docs
          @?= [ Document "f" source1
              , Document "o" source1
              , Document "o" source1
              , Document "b" source2
              , Document "a" source2
              , Document "r" source2
              , Document "b" source1
              , Document "a" source1
              , Document "z" source1
              ]
    , testCase "mergeSplits merges splits with separator" $
        mergeSplits 9 2 " " ["foo", "bar", "baz"]
          @?= ["foo bar", "baz"]
    ]

wordSplitterOps :: RecursiveCharacterSplitterOps
wordSplitterOps =
  defaultRecursiveCharacterSplitterOps
    { chunkSize = 3
    , chunkOverlap = 0
    , separators = [" ", ""]
    , keepSeparator = KeepSeparatorNone
    }

overlappingWordSplitterOps :: RecursiveCharacterSplitterOps
overlappingWordSplitterOps =
  wordSplitterOps {chunkSize = 5}

overlapSplitterOps :: RecursiveCharacterSplitterOps
overlapSplitterOps =
  defaultRecursiveCharacterSplitterOps
    { chunkSize = 7
    , chunkOverlap = 3
    , separators = [" ", ""]
    , keepSeparator = KeepSeparatorNone
    }

characterSplitterOps :: RecursiveCharacterSplitterOps
characterSplitterOps =
  defaultRecursiveCharacterSplitterOps
    { chunkSize = 1
    , chunkOverlap = 0
    , separators = [""]
    , keepSeparator = KeepSeparatorNone
    }
