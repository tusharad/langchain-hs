{- |
Module      : Langchain.Error
Description : Central error handling for langchain-hs re-exporting Langchain.Core.Error
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Re-exports structured error types and constructors from Langchain.Core.Error.
-}
module Langchain.Error
  ( module Langchain.Core.Error
  , LangchainResult
  , LangchainIO
  , fromString
  , toString
  , toText
  ) where

import Control.Exception (displayException)
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Core.Error

-- | Type alias for Either LangchainError a
type LangchainResult a = Either LangchainError a

-- | Type alias for IO (LangchainResult a)
type LangchainIO a = IO (LangchainResult a)

-- | Construct an InternalError from a String
fromString :: String -> LangchainError
fromString str = internalError (T.pack str) Nothing Nothing

-- | Convert a LangchainError to String via displayException
toString :: LangchainError -> String
toString = displayException

-- | Convert a LangchainError to Text via displayException
toText :: LangchainError -> Text
toText = T.pack . displayException
