{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Property.TextSplitterSpec (tests) where

import Data.Int (Int64)
import qualified Data.Text.Lazy as TL
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import Langchain.TextSplitter.Character

newtype SplitterText = SplitterText TL.Text
  deriving (Show, Eq)

instance Arbitrary SplitterText where
  arbitrary = do
    paragraphs <- listOf1 (listOf1 (elements (['a' .. 'z'] ++ ['0' .. '9'] ++ " ")))
    pure $ SplitterText $ TL.pack $ unlines paragraphs

newtype PositiveChunkSize = PositiveChunkSize Int64
  deriving (Show, Eq)

instance Arbitrary PositiveChunkSize where
  arbitrary = PositiveChunkSize . fromIntegral <$> chooseInt (10, 200)

tests :: TestTree
tests =
  testGroup
    "Langchain.Property.TextSplitterSpec (QuickCheck)"
    [ testProperty "Empty text splits into empty list" $
        \(PositiveChunkSize cSize) ->
          let ops = defaultCharacterSplitterOps {chunkSize = cSize}
           in splitText ops "" === []
    , testProperty "No chunk exceeds chunkSize" $
        \(PositiveChunkSize cSize) (SplitterText txt) ->
          let ops = defaultCharacterSplitterOps {chunkSize = cSize}
              chunks = splitText ops txt
           in property (all (\c -> TL.length c <= cSize) chunks)
    , testProperty "All generated chunks are non-empty" $
        \(PositiveChunkSize cSize) (SplitterText txt) ->
          let ops = defaultCharacterSplitterOps {chunkSize = cSize}
              chunks = splitText ops txt
           in property (all (not . TL.null) chunks)
    , testProperty "Single character chunks never exceed chunkSize 1" $
        \() ->
          let ops = defaultCharacterSplitterOps {chunkSize = 1, separator = ""}
              chunks = splitText ops "abcdef"
           in property (all (\c -> TL.length c <= 1) chunks)
    ]
