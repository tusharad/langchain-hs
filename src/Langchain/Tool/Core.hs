{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : Langchain.Tool.Core
Description : Re-exports effect-polymorphic Tool from langchain-hs-core
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Re-exports 'Tool m', 'createTool', and 'toolToValue'.
-}
module Langchain.Tool.Core
  ( module Langchain.Core.Tool
  ) where

import Langchain.Core.Tool
