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
  ) where

import Langchain.Core.Error

-- | Type alias for Either LangchainError a
type LangchainResult a = Either LangchainError a
