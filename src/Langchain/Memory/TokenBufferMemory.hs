{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Memory.TokenBufferMemory
Description : Token based Memory management for LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Implementation of LangChain's Conversation token buffer.
https://python.langchain.com/v0.1/docs/modules/memory/types/token_buffer/
-}
module Langchain.Memory.TokenBufferMemory
  ( TokenBufferMemory (..)
  , countTokens
  ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Langchain.LLM.Core
  ( ChatMessage
  , Message (..)
  , Role (..)
  , defaultMessageData
  )
import Langchain.Memory.Core
import Langchain.Runnable.Core (Runnable (..))

-- | Token based sliding window memory type
data TokenBufferMemory = TokenBufferMemory
  { maxTokens :: Int
  -- ^ Max number of tokens. 4 characters = 1 Token
  , tokenBufferMessages :: ChatMessage
  -- ^ Chat history (Nonempty List of Message)
  }
  deriving (Eq, Show)

{- | Function for counting tokens for the given list of messages
| 1 token = 4 characters
-}
countTokens :: [Message] -> Int
countTokens = sum . map go
  where
    go :: Message -> Int
    go (Message _ content _) =
      ceiling @Double
        (fromIntegral (T.length content) / 4.0)

instance BaseMemory TokenBufferMemory where
  messages TokenBufferMemory {..} = pure $ Right tokenBufferMessages
  addMessage t@TokenBufferMemory {..} newMsg = do
    let newMsgTokenCount = countTokens [newMsg]
        currentMsgsTokenCount = countTokens $ NE.toList tokenBufferMessages
    if newMsgTokenCount > maxTokens
      then
        pure (Left "New message is exceeding limit")
      else
        if newMsgTokenCount + currentMsgsTokenCount <= maxTokens
          then
            pure
              ( Right $
                  t
                    { tokenBufferMessages =
                        tokenBufferMessages <> NE.fromList [newMsg]
                    }
              )
          else
            trimNonSystemMsgs
              (NE.toList tokenBufferMessages)
              newMsgTokenCount
    where
      trimNonSystemMsgs msgs newMsgTokenCount = do
        let trimmedMsgs = removeOldestNonSystem msgs
        if trimmedMsgs == msgs -- If no more non sys msg left
          then
            pure
              ( Left $
                  "Cannot add new message since system message and "
                    <> "new message excedds limit"
              )
          else
            if countTokens trimmedMsgs + newMsgTokenCount <= maxTokens
              then
                pure
                  ( Right $
                      t
                        { tokenBufferMessages =
                            NE.fromList $ trimmedMsgs <> [newMsg]
                        }
                  )
              else trimNonSystemMsgs trimmedMsgs newMsgTokenCount

      removeOldestNonSystem = go
        where
          go [] = []
          go (m : ms)
            | isSystem m = m : go ms
            | otherwise = ms

      isSystem (Message role _ _) = role == System

  addUserMessage tokBuffMem uMsg =
    addMessage tokBuffMem (Message User uMsg defaultMessageData)

  addAiMessage tokBuffMem uMsg =
    addMessage tokBuffMem (Message Assistant uMsg defaultMessageData)

  clear tokBuffMem =
    pure $
      Right $
        tokBuffMem
          { tokenBufferMessages =
              NE.singleton $
                Message System "You are an AI model" defaultMessageData
          }

instance Runnable TokenBufferMemory where
  type RunnableInput TokenBufferMemory = T.Text
  type RunnableOutput TokenBufferMemory = TokenBufferMemory

  invoke = addUserMessage
