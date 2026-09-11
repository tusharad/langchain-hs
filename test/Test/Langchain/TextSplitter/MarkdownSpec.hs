{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.TextSplitter.MarkdownSpec (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as TL
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.TextSplitter.Markdown

tests :: TestTree
tests =
  testGroup
    "Langchain.TextSplitter.MarkdownSpec"
    [ testCase "Splits markdown and preserves header hierarchy in metadata" $ do
        let doc =
              "# Title\n\nIntroductory text.\n\n## Section 1\n\nSection 1 details.\n\n### SubSection A\n\nSubSection content.\n\n## Section 2\n\nSection 2 details."
            chunks = splitMarkdownToChunks defaultMarkdownSplitterOps doc
        case chunks of
          [c1, _c2, subSecChunk, _c4] -> do
            chunkHeaders c1 @?= Map.singleton "Header 1" "Title"
            Map.lookup "Header 1" (chunkHeaders subSecChunk) @?= Just "Title"
            Map.lookup "Header 2" (chunkHeaders subSecChunk) @?= Just "Section 1"
            Map.lookup "Header 3" (chunkHeaders subSecChunk) @?= Just "SubSection A"
          _ -> assertFailure ("Expected 4 chunks, got " ++ show (length chunks))
    , testCase "Section 2 clears previous subsection headers" $ do
        let doc =
              "# Title\n\n## Section 1\n\n### SubSection\n\nDetails.\n\n## Section 2\n\nNew section."
            chunks = splitMarkdownToChunks defaultMarkdownSplitterOps doc
        case chunks of
          [_, _, _, sec2Chunk] -> do
            Map.lookup "Header 2" (chunkHeaders sec2Chunk) @?= Just "Section 2"
            Map.lookup "Header 3" (chunkHeaders sec2Chunk) @?= Nothing
          _ -> assertFailure ("Expected 4 chunks, got " ++ show (length chunks))
    , testCase "Plain text markdown splitting produces non-empty chunks" $ do
        let doc = "# Main\n\nBody paragraph 1.\n\n## Sub\n\nBody paragraph 2."
            chunks = splitMarkdown defaultMarkdownSplitterOps doc
        case chunks of
          (firstChunk : _) -> do
            length chunks @?= 2
            assertBool "Chunk contains Main" ("Main" `TL.isInfixOf` firstChunk)
          [] -> assertFailure "Expected chunks to be non-empty"
    ]
