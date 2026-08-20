{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.TextSplitter.Character (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Langchain.TextSplitter.Character

tests :: TestTree
tests =
  testGroup
    "Langchain.TextSplitter.Character Tests"
    [ testCase "defaultCharacterSplitterOps should have correct values" $ do
        chunkSize defaultCharacterSplitterOps @?= 100
        separator defaultCharacterSplitterOps @?= "\n\n"
    , testCase "splitText should return empty list for empty text" $
        splitText defaultCharacterSplitterOps "" @?= []
    , testCase "splitText should keep text as single chunk if smaller than chunk size" $ do
        let text = "This is a small text"
            ops = defaultCharacterSplitterOps
        splitText ops text @?= [text]
    , testCase "splitText should split text by separator" $ do
        let text = "Paragraph 1\n\nParagraph 2\n\nParagraph 3"
            ops = defaultCharacterSplitterOps
        splitText ops text @?= ["Paragraph 1", "Paragraph 2", "Paragraph 3"]
    , testCase "splitText should split text by chunk size" $ do
        let text =
              "This is a very long text that should be split into chunks because it exceeds the chunk size limit."
            ops = CharacterSplitterOps {chunkSize = 20, separator = "\n\n"}
        splitText ops text
          @?= [ "This is a very long "
              , "text that should be "
              , "split into chunks be"
              , "cause it exceeds the"
              , " chunk size limit."
              ]
    , testCase "splitText should handle both separator and chunk size" $ do
        let text =
              "First paragraph that is quite long.\n\nSecond paragraph that is also very long and should be split."
            ops = CharacterSplitterOps {chunkSize = 20, separator = "\n\n"}
        splitText ops text
          @?= [ "First paragraph that"
              , " is quite long."
              , "Second paragraph tha"
              , "t is also very long "
              , "and should be split."
              ]
    , testCase "splitText should work with custom separator" $ do
        let text = "Item 1|Item 2|Item 3|Item 4"
            ops = CharacterSplitterOps {chunkSize = 100, separator = "|"}
        splitText ops text @?= ["Item 1", "Item 2", "Item 3", "Item 4"]
    , testCase "splitText should handle text with no separators" $ do
        let text =
              "ThisisasinglewordwithoutanyseparatorsthatshouldstillbesplitintochunksbasedonthechunksizeAlthoughithasnoseparatorsitcanstillbesplitproperly"
            ops = CharacterSplitterOps {chunkSize = 20, separator = "|"}
        splitText ops text
          @?= [ "Thisisasinglewordwit"
              , "houtanyseparatorstha"
              , "tshouldstillbespliti"
              , "ntochunksbasedonthec"
              , "hunksizeAlthoughitha"
              , "snoseparatorsitcanst"
              , "illbesplitproperly"
              ]
    , testCase "splitText should handle multiple adjacent separators" $ do
        let text = "Item 1\n\n\n\nItem 2\n\nItem 3"
            ops = defaultCharacterSplitterOps
        splitText ops text @?= ["Item 1", "Item 2", "Item 3"]
    , testCase "splitText should handle text starting with separators" $ do
        let text = "\n\nItem 1\n\nItem 2"
            ops = defaultCharacterSplitterOps
        splitText ops text @?= ["Item 1", "Item 2"]
    , testCase "splitText should handle text ending with separators" $ do
        let text = "Item 1\n\nItem 2\n\n"
            ops = defaultCharacterSplitterOps
        splitText ops text @?= ["Item 1", "Item 2"]
    , testCase "splitText should handle small chunk size" $ do
        let text = "abc"
            ops = CharacterSplitterOps {chunkSize = 1, separator = "\n\n"}
        splitText ops text @?= ["a", "b", "c"]
    , testCase "splitText should handle chunk size zero" $ do
        let text = "test"
            ops = CharacterSplitterOps {chunkSize = 0, separator = "\n\n"}
        splitText ops text @?= []
    , testCase "splitText should handle empty separator" $ do
        let text = "test"
            ops = CharacterSplitterOps {chunkSize = 2, separator = ""}
        splitText ops text @?= ["te", "st"]
    ]
