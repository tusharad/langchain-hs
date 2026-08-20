{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.Core.Error
Description : Core structured error types and context metadata
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Structured error handling without String-based error dropping.
-}
module Langchain.Core.Error
  ( LangchainError (..)
  , ErrorContext (..)
  , errorMessage
  , mkContext
  , mkContextIO
  , llmError
  , agentError
  , memoryError
  , toolError
  , vectorStoreError
  , documentLoaderError
  , embeddingError
  , runnableError
  , parsingError
  , networkError
  , configurationError
  , validationError
  , internalError
  ) where

import Control.DeepSeq (NFData)
import Control.Exception (Exception (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Generics (Generic)

-- | Detailed context metadata attached to every error.
data ErrorContext = ErrorContext
  { component :: Text
  , operation :: Text
  , timestamp :: UTCTime
  , details :: Map Text Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, NFData)

-- | Pure context constructor.
mkContext :: Text -> Text -> Map Text Text -> ErrorContext
mkContext comp op = ErrorContext comp op (posixSecondsToUTCTime 0)

-- | IO context constructor with real timestamp.
mkContextIO :: MonadIO m => Text -> Text -> Map Text Text -> m ErrorContext
mkContextIO comp op dt = do
  now <- liftIO getCurrentTime
  pure $ ErrorContext comp op now dt

-- | Core framework error type.
data LangchainError
  = LLMError Text (Maybe ErrorContext)
  | AgentError Text (Maybe ErrorContext)
  | MemoryError Text (Maybe ErrorContext)
  | ToolError Text (Maybe ErrorContext)
  | VectorStoreError Text (Maybe ErrorContext)
  | DocumentLoaderError Text (Maybe ErrorContext)
  | EmbeddingError Text (Maybe ErrorContext)
  | RunnableError Text (Maybe ErrorContext)
  | ParsingError Text (Maybe ErrorContext)
  | NetworkError Text (Maybe ErrorContext)
  | ConfigurationError Text (Maybe ErrorContext)
  | ValidationError Text (Maybe ErrorContext)
  | InternalError Text (Maybe ErrorContext)
  deriving (Show, Eq, Generic, ToJSON, FromJSON, NFData)

-- | Extract human-readable error message text from a LangchainError
errorMessage :: LangchainError -> Text
errorMessage (LLMError msg _) = msg
errorMessage (AgentError msg _) = msg
errorMessage (MemoryError msg _) = msg
errorMessage (ToolError msg _) = msg
errorMessage (VectorStoreError msg _) = msg
errorMessage (DocumentLoaderError msg _) = msg
errorMessage (EmbeddingError msg _) = msg
errorMessage (RunnableError msg _) = msg
errorMessage (ParsingError msg _) = msg
errorMessage (NetworkError msg _) = msg
errorMessage (ConfigurationError msg _) = msg
errorMessage (ValidationError msg _) = msg
errorMessage (InternalError msg _) = msg

instance Exception LangchainError where
  displayException err = case err of
    LLMError msg ctx -> formatError "LLMError" msg ctx
    AgentError msg ctx -> formatError "AgentError" msg ctx
    MemoryError msg ctx -> formatError "MemoryError" msg ctx
    ToolError msg ctx -> formatError "ToolError" msg ctx
    VectorStoreError msg ctx -> formatError "VectorStoreError" msg ctx
    DocumentLoaderError msg ctx -> formatError "DocumentLoaderError" msg ctx
    EmbeddingError msg ctx -> formatError "EmbeddingError" msg ctx
    RunnableError msg ctx -> formatError "RunnableError" msg ctx
    ParsingError msg ctx -> formatError "ParsingError" msg ctx
    NetworkError msg ctx -> formatError "NetworkError" msg ctx
    ConfigurationError msg ctx -> formatError "ConfigurationError" msg ctx
    ValidationError msg ctx -> formatError "ValidationError" msg ctx
    InternalError msg ctx -> formatError "InternalError" msg ctx
    where
      formatError errType msg Nothing = errType ++ ": " ++ T.unpack msg
      formatError errType msg (Just ctx) =
        errType
          ++ ": "
          ++ T.unpack msg
          ++ " [Component: "
          ++ T.unpack (component ctx)
          ++ ", Operation: "
          ++ T.unpack (operation ctx)
          ++ "]"

-- | Helper constructors
mkErrorCtx ::
  (Text -> Maybe ErrorContext -> LangchainError) -> Text -> Maybe Text -> Maybe Text -> LangchainError
mkErrorCtx ctor msg mbComp mbOp =
  let mbCtx = case (mbComp, mbOp) of
        (Nothing, Nothing) -> Nothing
        (Just c, Just o) -> Just $ mkContext c o mempty
        (Just c, Nothing) -> Just $ mkContext c "unspecified" mempty
        (Nothing, Just o) -> Just $ mkContext "unspecified" o mempty
   in ctor msg mbCtx

llmError :: Text -> Maybe Text -> Maybe Text -> LangchainError
llmError = mkErrorCtx LLMError

agentError :: Text -> Maybe Text -> Maybe Text -> LangchainError
agentError = mkErrorCtx AgentError

memoryError :: Text -> Maybe Text -> Maybe Text -> LangchainError
memoryError = mkErrorCtx MemoryError

toolError :: Text -> Maybe Text -> Maybe Text -> LangchainError
toolError = mkErrorCtx ToolError

vectorStoreError :: Text -> Maybe Text -> Maybe Text -> LangchainError
vectorStoreError = mkErrorCtx VectorStoreError

documentLoaderError :: Text -> Maybe Text -> Maybe Text -> LangchainError
documentLoaderError = mkErrorCtx DocumentLoaderError

embeddingError :: Text -> Maybe Text -> Maybe Text -> LangchainError
embeddingError = mkErrorCtx EmbeddingError

runnableError :: Text -> Maybe Text -> Maybe Text -> LangchainError
runnableError = mkErrorCtx RunnableError

parsingError :: Text -> Maybe Text -> Maybe Text -> LangchainError
parsingError = mkErrorCtx ParsingError

networkError :: Text -> Maybe Text -> Maybe Text -> LangchainError
networkError = mkErrorCtx NetworkError

configurationError :: Text -> Maybe Text -> Maybe Text -> LangchainError
configurationError = mkErrorCtx ConfigurationError

validationError :: Text -> Maybe Text -> Maybe Text -> LangchainError
validationError = mkErrorCtx ValidationError

internalError :: Text -> Maybe Text -> Maybe Text -> LangchainError
internalError = mkErrorCtx InternalError
