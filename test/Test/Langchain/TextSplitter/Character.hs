{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.TextSplitter.Character (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Langchain.TextSplitter.Character

tests :: TestTree
tests =
  testGroup
    "Langchain.TextSplitter.Character Tests"
    [ testCase "splitText returns empty list for empty text" $
        splitText defaultCharacterSplitterOps "" @?= []
    , testCase "splitText keeps small text as single chunk" $
        splitText defaultCharacterSplitterOps "This is a small text" @?= ["This is a small text"]
    , testCase "splitText splits on separator" $ do
        let ops = defaultCharacterSplitterOps
        splitText ops "Paragraph 1\n\nParagraph 2\n\nParagraph 3"
          @?= ["Paragraph 1", "Paragraph 2", "Paragraph 3"]
    , testCase "splitText splits long text by chunk size when no separator matches" $ do
        let ops = CharacterSplitterOps {chunkSize = 20, separator = "|"}
        splitText ops "Thisisasinglewordwithoutanyseparators"
          @?= ["Thisisasinglewordwit", "houtanyseparators"]
    , testCase "splitText handles both separator and chunk size" $ do
        let ops = CharacterSplitterOps {chunkSize = 20, separator = "\n\n"}
        splitText
          ops
          "First paragraph that is quite long.\n\nSecond paragraph that is also very long and should be split."
          @?= [ "First paragraph that"
              , " is quite long."
              , "Second paragraph tha"
              , "t is also very long "
              , "and should be split."
              ]
    , testCase "splitText strips empty chunks from adjacent separators" $ do
        splitText defaultCharacterSplitterOps "Item 1\n\n\n\nItem 2\n\nItem 3"
          @?= ["Item 1", "Item 2", "Item 3"]
    , testCase "splitText handles custom pipe separator" $ do
        let ops = CharacterSplitterOps {chunkSize = 100, separator = "|"}
        splitText ops "Item 1|Item 2|Item 3" @?= ["Item 1", "Item 2", "Item 3"]
    , testCase "splitText with empty separator splits by character chunk" $ do
        let ops = CharacterSplitterOps {chunkSize = 2, separator = ""}
        splitText ops "test" @?= ["te", "st"]
    ]
