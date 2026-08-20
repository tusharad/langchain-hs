{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Property.StateReducerSpec (tests) where

import qualified Data.Text as T
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import Langchain.Core.Model (Message, userMessage)
import Langchain.Graph.StateGraph (appendMessagesReducer, replaceFieldReducer)

newtype MsgList = MsgList [Message]
  deriving (Show, Eq)

instance Arbitrary MsgList where
  arbitrary = do
    txts <- listOf (T.pack <$> listOf1 (elements ['a' .. 'z']))
    pure $ MsgList (map userMessage txts)

tests :: TestTree
tests =
  testGroup
    "Langchain.Property.StateReducerSpec (QuickCheck)"
    [ testProperty "appendMessagesReducer Associativity: (a <> b) <> c == a <> (b <> c)" $
        \(MsgList a) (MsgList b) (MsgList c) ->
          let ab_c = appendMessagesReducer (appendMessagesReducer a b) c
              a_bc = appendMessagesReducer a (appendMessagesReducer b c)
           in ab_c === a_bc
    , testProperty "appendMessagesReducer Identity: a <> [] == a && [] <> a == a" $
        \(MsgList a) ->
          let rightId = appendMessagesReducer a []
              leftId = appendMessagesReducer [] a
           in (rightId === a) .&&. (leftId === a)
    , testProperty "replaceFieldReducer replaces old state with new state" $
        \s1 s2 ->
          replaceFieldReducer (s1 :: String) (s2 :: String) === s2
    ]
