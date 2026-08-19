{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Langchain.Property.ErrorSpec (tests) where

import Control.Exception (displayException)
import Data.Aeson (decode, encode)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import Langchain.Core.Error

instance Arbitrary ErrorContext where
  arbitrary = do
    comp <- T.pack <$> listOf1 (elements ['a' .. 'z'])
    op <- T.pack <$> listOf1 (elements ['a' .. 'z'])
    pure $ ErrorContext comp op (posixSecondsToUTCTime 1700000000) Map.empty

instance Arbitrary LangchainError where
  arbitrary = do
    msg <- T.pack <$> listOf1 (elements (['a' .. 'z'] ++ ['0' .. '9'] ++ " ,.-"))
    mbCtx <- oneof [pure Nothing, Just <$> arbitrary]
    elements
      [ LLMError msg mbCtx
      , AgentError msg mbCtx
      , MemoryError msg mbCtx
      , ToolError msg mbCtx
      , VectorStoreError msg mbCtx
      , DocumentLoaderError msg mbCtx
      , EmbeddingError msg mbCtx
      , RunnableError msg mbCtx
      , ParsingError msg mbCtx
      , NetworkError msg mbCtx
      , ConfigurationError msg mbCtx
      , ValidationError msg mbCtx
      , InternalError msg mbCtx
      ]

tests :: TestTree
tests =
  testGroup
    "Langchain.Property.ErrorSpec (QuickCheck)"
    [ testProperty "LangchainError JSON round-trip: decode (encode err) == Just err" $
        \err -> decode (encode (err :: LangchainError)) === Just err
    , testProperty "errorMessage returns non-empty message for non-empty error text" $
        \err ->
          property (not $ T.null $ errorMessage (err :: LangchainError))
    , testProperty "displayException includes error message" $
        \err ->
          let disp = displayException (err :: LangchainError)
              msg = T.unpack (errorMessage err)
           in property (msg `isInfixOf` disp)
    ]
