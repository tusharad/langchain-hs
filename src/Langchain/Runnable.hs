{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : Langchain.Runnable
Description : Re-exports pure RunnableTree pipeline AST from langchain-hs-core
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Re-exports pure GADT pipeline AST 'RunnableTree', interpretation engine 'interpret',
and composition operators '|>>' and '&>&'.
-}
module Langchain.Runnable
  ( module Langchain.Core.Runnable
  ) where

import Langchain.Core.Runnable
