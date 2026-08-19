{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.TextSplitter.RecursiveCharacterSpec (tests) where

import qualified Data.Text.Lazy as TL
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.TextSplitter.RecursiveCharacter

tests :: TestTree
tests =
  testGroup
    "Langchain.TextSplitter.RecursiveCharacterSpec"
    [ testCase "Empty text returns empty chunk list" $ do
        splitTextRecursive defaultRecursiveCharacterSplitterOps "" @?= []
    , testCase "Splits on paragraph separators first" $ do
        let text =
              "Paragraph 1 is about Haskell.\n\nParagraph 2 is about pure functions.\n\nParagraph 3 is about types."
            ops = defaultRecursiveCharacterSplitterOps {chunkSize = 40, chunkOverlap = 0}
            chunks = splitTextRecursive ops text
        assertBool "Multiple chunks produced" (length chunks >= 3)
        assertBool "No chunk exceeds max length" (all (\c -> TL.length c <= 40) chunks)
    , testCase "Respects chunk overlap" $ do
        let text = "First sentence here.\n\nSecond sentence here.\n\nThird sentence here."
            ops = defaultRecursiveCharacterSplitterOps {chunkSize = 45, chunkOverlap = 15}
            chunks = splitTextRecursive ops text
        assertBool "Produced chunks" (length chunks >= 2)
    , testCase "Fallback to character splitting when no separators match" $ do
        let text = TL.replicate 100 "a"
            ops = defaultRecursiveCharacterSplitterOps {chunkSize = 25, chunkOverlap = 0}
            chunks = splitTextRecursive ops text
        length chunks @?= 4
        assertBool "All chunks equal length 25" (all (\c -> TL.length c == 25) chunks)
    ]
