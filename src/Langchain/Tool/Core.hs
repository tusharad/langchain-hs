{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Module      : Langchain.Tool.Core
Description : Core Tool typeclass for LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Core module defining the Tool typeclass for Langchain-Haskell integration.

This module provides a typeclass interface for creating interoperable tools
that can be used with Large Language Models (LLMs) in Haskell applications.
The design mirrors LangChain's Python tooling system while maintaining
Haskell's type safety and functional programming principles.

Example use case:

> data Calculator = Calculator
>
> instance Tool Calculator where
>   type Input Calculator = (Int, Int)
>   type Output Calculator = Int
>   toolName _ = "calculator"
>   toolDescription _ = "Performs arithmetic operations on two integers"
>   runTool _ (a, b) = pure (a + b)
-}
module Langchain.Tool.Core
  ( Tool (..)
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)

{- | Typeclass defining the interface for tools that can be used with LLMs.

Tools represent capabilities that can be invoked by language models,
following the LangChain framework's tooling pattern. Each tool must:

* Define input/output types using type families
* Provide a unique name and description
* Implement an IO-based execution function

The use of type families allows for flexible yet type-safe tool composition,
while the IO monad accommodates both pure and effectful implementations.
-}
class Tool a where
  {- | Input type required by the tool

  Example: For a weather lookup tool, this might be 'LocationCoordinates'
  -}
  type Input a

  {- | Output type produced by the tool

  Example: For a calculator tool, this could be 'Int' or 'Double'
  -}
  type Output a

  {- | Get the tool's unique identifier

  >>> toolName (undefined :: Calculator)
  "calculator"
  -}
  toolName :: a -> Text

  {- | Get human-readable description of the tool's purpose

  >>> toolDescription (undefined :: Calculator)
  "Performs arithmetic operations on two integers"
  -}
  toolDescription :: a -> Text

  {- | Execute the tool with given input

  This function bridges the gap between LLM abstractions and concrete
  implementations. The IO context allows for:

  * Pure computations (via 'pure')
  * External API calls
  * Database queries

  Example implementation:

  > runTool _ (a, b) = do
  >   putStrLn "Calculating..."
  >   pure (a + b)
  -}
  runTool :: a -> Input a -> IO (Output a)

  -- | MonadIO version of runTool
  runToolM :: MonadIO m => a -> Input a -> m (Output a)
  runToolM tool toolInput = liftIO $ runTool tool toolInput
