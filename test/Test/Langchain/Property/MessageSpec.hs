{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Langchain.Property.MessageSpec (tests) where

import Data.Aeson (decode, encode, toJSON)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import Langchain.Core.Model

-- Arbitrary instances for Core Message types

instance Arbitrary Role where
  arbitrary = elements [System, User, Assistant, Tool, Developer, Function]

instance Arbitrary ContentBlock where
  arbitrary =
    oneof
      [ TextBlock <$> (T.pack <$> listOf1 (elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ " \t\n.,!?-")))
      , ImageBlock <$> elements ["image/png", "image/jpeg", "image/webp"] <*> (T.pack <$> listOf1 (elements ['a' .. 'z']))
      , AudioBlock <$> elements ["audio/mp3", "audio/wav"] <*> (T.pack <$> listOf1 (elements ['a' .. 'z']))
      , DataBlock . BS.pack <$> listOf1 arbitrary
      ]

instance Arbitrary ToolCall where
  arbitrary = do
    tcId <- T.pack <$> listOf1 (elements ['a' .. 'z'])
    name <- T.pack <$> listOf1 (elements ['a' .. 'z'])
    pure $ ToolCall tcId "function" name (toJSON ("{}" :: Text))

instance Arbitrary Message where
  arbitrary = do
    r <- arbitrary
    blocks <- listOf1 arbitrary
    let neBlocks = NonEmpty.fromList blocks
    mbName <- oneof [pure Nothing, Just . T.pack <$> listOf1 (elements ['a' .. 'z'])]
    mbToolId <- oneof [pure Nothing, Just . T.pack <$> listOf1 (elements ['a' .. 'z'])]
    pure $ Message r neBlocks mbName Nothing mbToolId

tests :: TestTree
tests =
  testGroup
    "Langchain.Property.MessageSpec (QuickCheck)"
    [ testProperty "Role JSON round-trip: decode (encode r) == Just r" $
        \r -> decode (encode (r :: Role)) === Just r
    , testProperty "ContentBlock JSON round-trip: decode (encode cb) == Just cb" $
        \cb -> decode (encode (cb :: ContentBlock)) === Just cb
    , testProperty "Message JSON round-trip: decode (encode msg) == Just msg" $
        \msg -> decode (encode (msg :: Message)) === Just msg
    , testProperty "extractMessageText preserves text block contents" $
        \t ->
          let txt = T.pack t
              msg = userMessage txt
           in extractMessageText msg === txt
    ]
