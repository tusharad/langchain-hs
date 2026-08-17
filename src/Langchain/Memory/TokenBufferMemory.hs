{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Memory.TokenBufferMemory
Description : Token-based memory management for LangChain Haskell
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Thread-safe token buffer memory maintaining conversation history within a token budget.
-}
module Langchain.Memory.TokenBufferMemory
  ( TokenBufferMemory (..)
  , newTokenBufferMemory
  , countTokens
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO, writeTVar)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T

import Langchain.Core.Error (memoryError)
import Langchain.Core.Model
  ( Message (..)
  , Role (..)
  , extractMessageText
  , systemMessage
  )
import Langchain.Memory.Core

-- | Token-based sliding window memory type
data TokenBufferMemory = TokenBufferMemory
  { maxTokens :: !Int
  , memVar :: !(TVar [Message])
  }

instance Show TokenBufferMemory where
  show (TokenBufferMemory maxT _) = "TokenBufferMemory { maxTokens = " ++ show maxT ++ " }"

instance Eq TokenBufferMemory where
  (TokenBufferMemory t1 tv1) == (TokenBufferMemory t2 tv2) =
    t1 == t2 && tv1 == tv2

-- | Construct a new TokenBufferMemory
newTokenBufferMemory :: MonadIO m => Int -> [Message] -> m TokenBufferMemory
newTokenBufferMemory maxT initMsgs = liftIO $ do
  tv <- newTVarIO initMsgs
  pure $ TokenBufferMemory maxT tv

-- | Approximate token count: 4 characters ≈ 1 token
countTokens :: [Message] -> Int
countTokens = sum . map (\m -> ceiling (fromIntegral (T.length (extractMessageText m)) / (4.0 :: Double)))

instance BaseMemory TokenBufferMemory where
  messages (TokenBufferMemory _ tv) = liftIO $ readTVarIO tv

  addMessage (TokenBufferMemory maxT tv) newMsg = do
    let newMsgTokens = countTokens [newMsg]
    if newMsgTokens > maxT
      then throwError $ memoryError "New message exceeds maximum token limit" (Just "TokenBufferMemory") Nothing
      else liftIO $ atomically $ modifyTVar' tv $ \currMsgs ->
        trimToLimit currMsgs newMsgTokens [newMsg]
    where
      trimToLimit currMsgs newMsgToks acc =
        let candidate = currMsgs ++ acc
         in if countTokens candidate <= maxT
              then candidate
              else case removeOldestNonSystem currMsgs of
                Just trimmed -> trimToLimit trimmed newMsgToks acc
                Nothing -> [newMsg]

      removeOldestNonSystem [] = Nothing
      removeOldestNonSystem (m : ms)
        | messageRole m == System = fmap (m :) (removeOldestNonSystem ms)
        | otherwise = Just ms

  clear (TokenBufferMemory _ tv) = liftIO $ do
    atomically $ writeTVar tv [systemMessage "You are a helpful AI assistant"]
