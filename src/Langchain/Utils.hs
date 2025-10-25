{- |
Module      : Langchain.Utils
Description : Utility functions for LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

This module provides utility functions used throughout the LangChain Haskell library.
-}
module Langchain.Utils (showText) where

import Data.Text (Text, pack)

{- | Convert any 'Show' instance to 'Text'
Convenience function for converting values to Text format.

Example:

>>> showText (42 :: Int)
"42"

>>> showText (True)
"True"
-}
showText :: Show a => a -> Text
showText = pack . show
