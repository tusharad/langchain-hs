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
    [ testCase "Empty text returns empty chunk list" $
        splitTextRecursive defaultRecursiveCharacterSplitterOps "" @?= []
    , testCase "Keeps text as a single chunk when it fits chunkSize" $ do
        let text = "short text"
            ops = defaultRecursiveCharacterSplitterOps {chunkSize = 100, chunkOverlap = 0}
        splitTextRecursive ops text @?= [text]
    , testCase "Prefers paragraph separator before line separator" $ do
        let text = "aa\n\nbb\n\ncc"
            ops = defaultRecursiveCharacterSplitterOps {chunkSize = 8, chunkOverlap = 0}
        splitTextRecursive ops text @?= ["aa\n\nbb", "cc"]
    , testCase "Falls back to line separator when paragraph separator is absent" $ do
        let text = "aa\nbb\ncc"
            ops = defaultRecursiveCharacterSplitterOps {chunkSize = 6, chunkOverlap = 0}
        splitTextRecursive ops text @?= ["aa\nbb", "cc"]
    , testCase "Falls back to space separator when no newline separators match" $ do
        let text = "aa bb cc"
            ops = defaultRecursiveCharacterSplitterOps {chunkSize = 5, chunkOverlap = 0}
        splitTextRecursive ops text @?= ["aa bb", "cc"]
    , testCase "Falls back to character splitting when no separators match" $ do
        let text = TL.replicate 7 "a"
            ops = defaultRecursiveCharacterSplitterOps {chunkSize = 3, chunkOverlap = 0}
        splitTextRecursive ops text @?= ["aaa", "aaa", "a"]
    , testCase "Uses split-by-length fallback when separators list is empty" $ do
        let text = "abcdefgh"
            ops = defaultRecursiveCharacterSplitterOps {chunkSize = 3, chunkOverlap = 0, separators = []}
        splitTextRecursive ops text @?= ["abc", "def", "gh"]
    , testCase "Drops empty chunks caused by adjacent and edge separators" $ do
        let text = "\n\nA\n\n\n\nB\n\n"
            ops = defaultRecursiveCharacterSplitterOps {chunkSize = 3, chunkOverlap = 0}
        splitTextRecursive ops text @?= ["A", "B"]
    , testCase "Respects overlap with deterministic expected chunks" $ do
        let text = "ab|cd|ef|gh"
            ops =
              defaultRecursiveCharacterSplitterOps
                { chunkSize = 5
                , chunkOverlap = 2
                , separators = ["|", ""]
                }
        splitTextRecursive ops text @?= ["ab|cd", "cd|ef", "ef|gh"]
    ]
