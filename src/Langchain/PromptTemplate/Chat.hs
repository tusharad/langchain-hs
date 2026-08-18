{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.PromptTemplate.Chat
Description : Chat prompt template primitives
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Minimal chat prompt primitives ported from LangChain Python chat prompts.
-}
module Langchain.PromptTemplate.Chat
  ( BaseStringMessagePromptTemplate (..)
  , BaseMessagePromptTemplate (..)
  , extractTemplateVariables
  ) where

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model.Types (Message)
import Langchain.PromptTemplate.String (extractTemplateVariables)

-- | Base class for message prompt templates backed by string prompt templates.
class BaseStringMessagePromptTemplate template options | template -> options where
  fromTemplateFile :: FilePath -> options -> IO template

-- | Base class for message prompt templates.
class BaseMessagePromptTemplate template input where
  formatMessages :: template -> input -> Either LangchainError [Message]
