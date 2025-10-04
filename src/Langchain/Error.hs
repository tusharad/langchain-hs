{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Error
Description : Central error handling for langchain-hs
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

This module provides a comprehensive error handling system for langchain-hs,
replacing the previous `Either String` pattern with a structured, type-safe
approach that follows industry best practices.

The error system includes:

* Structured error types with context and metadata
* Error severity levels and categories
* Utility functions for error construction and handling
* Integration with existing langchain-hs components
* Support for error chaining and context preservation

Example usage:

@
import Langchain.Error

-- Creating errors
let err = llmError "Model timeout" (Just "gpt-4") Nothing

-- Error handling with context
result <- someOperation
case result of
  Left err -> do
    logError err
    handleError err
  Right value -> processValue value

-- Error chaining
chainError "Failed to process document" originalError
@
-}
module Langchain.Error
  ( -- * Error Types
    LangchainError (..)
  , ErrorSeverity (..)
  , ErrorCategory (..)
  , ErrorContext (..)

    -- * Error Construction
  , llmError
  , llmErrorWithContext
  , agentError
  , agentErrorWithContext
  , memoryError
  , memoryErrorWithContext
  , toolError
  , toolErrorWithContext
  , vectorStoreError
  , vectorStoreErrorWithContext
  , documentLoaderError
  , documentLoaderErrorWithContext
  , embeddingError
  , embeddingErrorWithContext
  , runnableError
  , runnableErrorWithContext
  , parsingError
  , parsingErrorWithContext
  , networkError
  , networkErrorWithContext
  , configurationError
  , configurationErrorWithContext
  , validationError
  , validationErrorWithContext
  , internalError
  , internalErrorWithContext

    -- * Error Utilities
  , chainError
  , addContext
  , withErrorContext
  , mapError
  , fromString
  , toString
  , toText
  , logError
  , isRetryable
  , getSeverity
  , getCategory
  , fromStringError
  , fromException
  , liftStringError
  , simpleError
  , catchToLangchainError
  , withContext
  , withContextIO

    -- * Type Aliases
  , LangchainResult
  , LangchainIO

    -- * Re-exports for convenience
  , module Control.Exception
  ) where

import Control.Exception (Exception, SomeException, displayException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.IO (hPutStrLn, stderr)

-- | Severity levels for errors, following industry standards
data ErrorSeverity
  = -- | System-breaking errors that require immediate attention
    Critical
  | -- | Errors that prevent core functionality
    High
  | -- | Errors that degrade functionality but allow continuation
    Medium
  | -- | Minor errors or warnings
    Low
  | -- | Informational messages
    Info
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | Categories of errors for better organization and handling
data ErrorCategory
  = -- | Language model related errors
    LLMError
  | -- | Agent execution errors
    AgentError
  | -- | Memory management errors
    MemoryError
  | -- | Tool execution errors
    ToolError
  | -- | Vector store operation errors
    VectorStoreError
  | -- | Document loading errors
    DocumentLoaderError
  | -- | Embedding generation errors
    EmbeddingError
  | -- | Runnable execution errors
    RunnableError
  | -- | Data parsing and validation errors
    ParsingError
  | -- | Network and HTTP errors
    NetworkError
  | -- | Configuration and setup errors
    ConfigurationError
  | -- | Input validation errors
    ValidationError
  | -- | Internal system errors
    InternalError
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Additional context information for errors
data ErrorContext = ErrorContext
  { contextComponent :: Maybe Text
  -- ^ Component where error occurred
  , contextOperation :: Maybe Text
  -- ^ Operation being performed
  , contextInput :: Maybe Text
  -- ^ Input that caused the error
  , contextMetadata :: [(Text, Text)]
  -- ^ Additional metadata
  , contextTimestamp :: UTCTime
  -- ^ When the error occurred
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | The central error type for langchain-hs
data LangchainError = LangchainError
  { errorMessage :: Text
  -- ^ Human-readable error message
  , errorSeverity :: ErrorSeverity
  -- ^ Severity level
  , errorCategory :: ErrorCategory
  -- ^ Error category
  , errorContext :: Maybe ErrorContext
  -- ^ Additional context
  , errorCause :: Maybe LangchainError
  -- ^ Chained/nested error
  , errorCode :: Maybe Text
  -- ^ Optional error code
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Exception LangchainError where
  displayException LangchainError {..} =
    T.unpack $
      T.unlines $
        filter
          (not . T.null)
          [ "["
              <> T.pack (show errorSeverity)
              <> "] "
              <> T.pack (show errorCategory)
              <> ": "
              <> errorMessage
          , maybe "" ("Error Code: " <>) errorCode
          , maybe "" formatContext errorContext
          , maybe "" (\cause -> "Caused by: " <> T.pack (show cause)) errorCause
          ]
    where
      formatContext ErrorContext {..} =
        T.unlines $
          filter
            (not . T.null)
            [ maybe "" ("Component: " <>) contextComponent
            , maybe "" ("Operation: " <>) contextOperation
            , maybe "" ("Input: " <>) contextInput
            , if null contextMetadata then "" else "Metadata: " <> T.pack (show contextMetadata)
            , "Timestamp: " <> T.pack (show contextTimestamp)
            ]

-- | Type alias for results that can fail with LangchainError
type LangchainResult a = Either LangchainError a

-- | Type alias for IO operations that can fail with LangchainError
type LangchainIO a = IO (LangchainResult a)

-- | Create an LLM-related error
llmError :: Text -> Maybe Text -> Maybe Text -> LangchainError
llmError msg _model _operation =
  LangchainError
    { errorMessage = msg
    , errorSeverity = High
    , errorCategory = LLMError
    , errorContext = Nothing
    , errorCause = Nothing
    , errorCode = Nothing
    }

-- | Create an LLM error with context
llmErrorWithContext ::
  Text ->
  Maybe Text ->
  Maybe Text ->
  ErrorContext ->
  LangchainError
llmErrorWithContext msg model operation ctx =
  (llmError msg model operation)
    { errorContext =
        Just ctx {contextComponent = model, contextOperation = operation}
    }

-- | Create an agent-related error
agentError :: Text -> Maybe Text -> Maybe Text -> LangchainError
agentError msg _agentType _operation =
  LangchainError
    { errorMessage = msg
    , errorSeverity = High
    , errorCategory = AgentError
    , errorContext = Nothing
    , errorCause = Nothing
    , errorCode = Nothing
    }

-- | Create an agent error with context
agentErrorWithContext :: Text -> Maybe Text -> Maybe Text -> ErrorContext -> LangchainError
agentErrorWithContext msg agentType operation ctx =
  (agentError msg agentType operation)
    { errorContext = Just ctx {contextComponent = agentType, contextOperation = operation}
    }

-- | Create a memory-related error
memoryError :: Text -> Maybe Text -> Maybe Text -> LangchainError
memoryError msg _memoryType _operation =
  LangchainError
    { errorMessage = msg
    , errorSeverity = Medium
    , errorCategory = MemoryError
    , errorContext = Nothing
    , errorCause = Nothing
    , errorCode = Nothing
    }

-- | Create a memory error with context
memoryErrorWithContext :: Text -> Maybe Text -> Maybe Text -> ErrorContext -> LangchainError
memoryErrorWithContext msg memoryType operation ctx =
  (memoryError msg memoryType operation)
    { errorContext = Just ctx {contextComponent = memoryType, contextOperation = operation}
    }

-- | Create a tool-related error
toolError :: Text -> Maybe Text -> Maybe Text -> LangchainError
toolError msg _toolName _operation =
  LangchainError
    { errorMessage = msg
    , errorSeverity = High
    , errorCategory = ToolError
    , errorContext = Nothing
    , errorCause = Nothing
    , errorCode = Nothing
    }

-- | Create a tool error with context
toolErrorWithContext :: Text -> Maybe Text -> Maybe Text -> ErrorContext -> LangchainError
toolErrorWithContext msg toolName operation ctx =
  (toolError msg toolName operation)
    { errorContext = Just ctx {contextComponent = toolName, contextOperation = operation}
    }

-- | Create a vector store error
vectorStoreError :: Text -> Maybe Text -> Maybe Text -> LangchainError
vectorStoreError msg _storeType _operation =
  LangchainError
    { errorMessage = msg
    , errorSeverity = High
    , errorCategory = VectorStoreError
    , errorContext = Nothing
    , errorCause = Nothing
    , errorCode = Nothing
    }

-- | Create a vector store error with context
vectorStoreErrorWithContext :: Text -> Maybe Text -> Maybe Text -> ErrorContext -> LangchainError
vectorStoreErrorWithContext msg storeType operation ctx =
  (vectorStoreError msg storeType operation)
    { errorContext = Just ctx {contextComponent = storeType, contextOperation = operation}
    }

-- | Create a document loader error
documentLoaderError :: Text -> Maybe Text -> Maybe Text -> LangchainError
documentLoaderError msg _loaderType _operation =
  LangchainError
    { errorMessage = msg
    , errorSeverity = Medium
    , errorCategory = DocumentLoaderError
    , errorContext = Nothing
    , errorCause = Nothing
    , errorCode = Nothing
    }

-- | Create a document loader error with context
documentLoaderErrorWithContext :: Text -> Maybe Text -> Maybe Text -> ErrorContext -> LangchainError
documentLoaderErrorWithContext msg loaderType operation ctx =
  (documentLoaderError msg loaderType operation)
    { errorContext = Just ctx {contextComponent = loaderType, contextOperation = operation}
    }

-- | Create an embedding error
embeddingError :: Text -> Maybe Text -> Maybe Text -> LangchainError
embeddingError msg _embeddingType _operation =
  LangchainError
    { errorMessage = msg
    , errorSeverity = High
    , errorCategory = EmbeddingError
    , errorContext = Nothing
    , errorCause = Nothing
    , errorCode = Nothing
    }

-- | Create an embedding error with context
embeddingErrorWithContext :: Text -> Maybe Text -> Maybe Text -> ErrorContext -> LangchainError
embeddingErrorWithContext msg embeddingType operation ctx =
  (embeddingError msg embeddingType operation)
    { errorContext = Just ctx {contextComponent = embeddingType, contextOperation = operation}
    }

-- | Create a runnable error
runnableError :: Text -> Maybe Text -> Maybe Text -> LangchainError
runnableError msg _runnableType _operation =
  LangchainError
    { errorMessage = msg
    , errorSeverity = High
    , errorCategory = RunnableError
    , errorContext = Nothing
    , errorCause = Nothing
    , errorCode = Nothing
    }

-- | Create a runnable error with context
runnableErrorWithContext :: Text -> Maybe Text -> Maybe Text -> ErrorContext -> LangchainError
runnableErrorWithContext msg runnableType operation ctx =
  (runnableError msg runnableType operation)
    { errorContext = Just ctx {contextComponent = runnableType, contextOperation = operation}
    }

-- | Create a parsing error
parsingError :: Text -> Maybe Text -> Maybe Text -> LangchainError
parsingError msg _parserType _input =
  LangchainError
    { errorMessage = msg
    , errorSeverity = Medium
    , errorCategory = ParsingError
    , errorContext = Nothing
    , errorCause = Nothing
    , errorCode = Nothing
    }

-- | Create a parsing error with context
parsingErrorWithContext :: Text -> Maybe Text -> Maybe Text -> ErrorContext -> LangchainError
parsingErrorWithContext msg parserType input ctx =
  (parsingError msg parserType input)
    { errorContext = Just ctx {contextComponent = parserType, contextInput = input}
    }

-- | Create a network error
networkError :: Text -> Maybe Text -> Maybe Text -> LangchainError
networkError msg _endpoint _operation =
  LangchainError
    { errorMessage = msg
    , errorSeverity = High
    , errorCategory = NetworkError
    , errorContext = Nothing
    , errorCause = Nothing
    , errorCode = Nothing
    }

-- | Create a network error with context
networkErrorWithContext :: Text -> Maybe Text -> Maybe Text -> ErrorContext -> LangchainError
networkErrorWithContext msg endpoint operation ctx =
  (networkError msg endpoint operation)
    { errorContext = Just ctx {contextComponent = endpoint, contextOperation = operation}
    }

-- | Create a configuration error
configurationError :: Text -> Maybe Text -> Maybe Text -> LangchainError
configurationError msg _configKey _operation =
  LangchainError
    { errorMessage = msg
    , errorSeverity = Critical
    , errorCategory = ConfigurationError
    , errorContext = Nothing
    , errorCause = Nothing
    , errorCode = Nothing
    }

-- | Create a configuration error with context
configurationErrorWithContext :: Text -> Maybe Text -> Maybe Text -> ErrorContext -> LangchainError
configurationErrorWithContext msg configKey operation ctx =
  (configurationError msg configKey operation)
    { errorContext = Just ctx {contextComponent = configKey, contextOperation = operation}
    }

-- | Create a validation error
validationError :: Text -> Maybe Text -> Maybe Text -> LangchainError
validationError msg _field _input =
  LangchainError
    { errorMessage = msg
    , errorSeverity = Medium
    , errorCategory = ValidationError
    , errorContext = Nothing
    , errorCause = Nothing
    , errorCode = Nothing
    }

-- | Create a validation error with context
validationErrorWithContext :: Text -> Maybe Text -> Maybe Text -> ErrorContext -> LangchainError
validationErrorWithContext msg field input ctx =
  (validationError msg field input)
    { errorContext = Just ctx {contextComponent = field, contextInput = input}
    }

-- | Create an internal error
internalError :: Text -> Maybe Text -> Maybe Text -> LangchainError
internalError msg _component _operation =
  LangchainError
    { errorMessage = msg
    , errorSeverity = Critical
    , errorCategory = InternalError
    , errorContext = Nothing
    , errorCause = Nothing
    , errorCode = Nothing
    }

-- | Create an internal error with context
internalErrorWithContext :: Text -> Maybe Text -> Maybe Text -> ErrorContext -> LangchainError
internalErrorWithContext msg component operation ctx =
  (internalError msg component operation)
    { errorContext = Just ctx {contextComponent = component, contextOperation = operation}
    }

-- | Chain an error with a new message, preserving the original as the cause
chainError :: Text -> LangchainError -> LangchainError
chainError msg originalError =
  LangchainError
    { errorMessage = msg
    , errorSeverity = errorSeverity originalError
    , errorCategory = errorCategory originalError
    , errorContext = errorContext originalError
    , errorCause = Just originalError
    , errorCode = errorCode originalError
    }

-- | Add context to an existing error
addContext :: ErrorContext -> LangchainError -> LangchainError
addContext ctx err = err {errorContext = Just ctx}

-- | Execute an action with error context, automatically adding context to any errors
withErrorContext :: MonadIO m => ErrorContext -> LangchainIO a -> m (LangchainResult a)
withErrorContext ctx action = liftIO $ do
  result <- action
  case result of
    Left err -> return $ Left $ addContext ctx err
    Right val -> return $ Right val

-- | Map a function over the error in a result
mapError :: (LangchainError -> LangchainError) -> LangchainResult a -> LangchainResult a
mapError f (Left err) = Left (f err)
mapError _ (Right val) = Right val

-- | Convert a String to LangchainError
fromString :: String -> LangchainError
fromString str = internalError (T.pack str) Nothing Nothing

-- | Convert LangchainError to String
toString :: LangchainError -> String
toString = displayException

-- | Convert LangchainError to Text
toText :: LangchainError -> Text
toText = T.pack . toString

-- | Log an error to stderr (can be extended to use proper logging)
logError :: MonadIO m => LangchainError -> m ()
logError err = liftIO $ hPutStrLn stderr $ toString err

-- | Check if an error is retryable based on its category and severity
isRetryable :: LangchainError -> Bool
isRetryable LangchainError {..} = case errorCategory of
  NetworkError -> errorSeverity <= High
  LLMError -> errorSeverity <= Medium
  VectorStoreError -> errorSeverity <= Medium
  EmbeddingError -> errorSeverity <= Medium
  ToolError -> errorSeverity <= Medium
  _ -> False

-- | Get the severity of an error
getSeverity :: LangchainError -> ErrorSeverity
getSeverity = errorSeverity

-- | Get the category of an error
getCategory :: LangchainError -> ErrorCategory
getCategory = errorCategory

-- | Convert a String error to LangchainError (for backward compatibility)
fromStringError :: String -> LangchainError
fromStringError = fromString

-- | Convert an IO exception to LangchainError
fromException :: SomeException -> LangchainError
fromException ex = internalError (T.pack $ displayException ex) Nothing Nothing

-- | Lift an Either String to LangchainResult
liftStringError :: Either String a -> LangchainResult a
liftStringError (Left err) = Left (fromString err)
liftStringError (Right val) = Right val

-- | Create a simple error with just a message (uses InternalError category)
simpleError :: Text -> LangchainError
simpleError msg = internalError msg Nothing Nothing

-- | Catch IO exceptions and convert them to LangchainError
catchToLangchainError :: IO a -> IO (LangchainResult a)
catchToLangchainError action = do
  result <- try action
  case result of
    Left ex -> return $ Left $ fromException ex
    Right val -> return $ Right val

-- | Run an action and add context to any errors
withContext :: Text -> Text -> LangchainResult a -> LangchainResult a
withContext component operation result = case result of
  Left err ->
    Left $
      err
        { errorContext =
            Just $
              ErrorContext
                { contextComponent = Just component
                , contextOperation = Just operation
                , contextInput = Nothing
                , contextMetadata = []
                , contextTimestamp = case errorContext err of
                    Just ctx -> contextTimestamp ctx
                    Nothing -> error "withContext requires timestamp - use withContextIO instead"
                }
        }
  Right val -> Right val

-- | Run an action and add context to any errors (IO version)
withContextIO :: MonadIO m => Text -> Text -> LangchainResult a -> m (LangchainResult a)
withContextIO component operation result = case result of
  Left err -> do
    now <- liftIO getCurrentTime
    return $
      Left $
        err
          { errorContext =
              Just $
                ErrorContext
                  { contextComponent = Just component
                  , contextOperation = Just operation
                  , contextInput = Nothing
                  , contextMetadata = []
                  , contextTimestamp = now
                  }
          }
  Right val -> return $ Right val
