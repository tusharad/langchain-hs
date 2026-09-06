{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Langchain.Tool.Binding
Description : Typeclass for attaching tool definitions to provider-specific model configs
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides 'ToolBinder' typeclass enabling agents to attach tool definitions
to provider-specific 'ModelConfig' types in a uniform way. This is the bridge
between provider-agnostic agent code and provider-specific tool APIs.
-}
module Langchain.Tool.Binding
  ( ToolBinder (..)
  ) where

import Langchain.Core.Model (ChatModel (..))
import Langchain.Core.Tool (Tool)

{- | Typeclass for models that support binding tools into their 'ModelConfig'.

Agents like 'ReActAgent' use this to pass tool definitions to the LLM
provider in a provider-agnostic way.

= Example

@
-- Agent code (provider-agnostic):
let cfg = bindToolsConfig tools Nothing
responseMsg <- invoke model history cfg

-- The right thing happens automatically:
-- For Ollama: builds a ChatRequest with chatTools set
-- For OpenAI: builds a Value with "tools" key
-- For OllamaWithTools: merges into existing config
@
-}
class (ChatModel model) => ToolBinder model m where
  {- | Convert a list of tools into a provider-specific 'ModelConfig',
  optionally merging with an existing config.
  -}
  bindToolsConfig :: [Tool m] -> Maybe (ModelConfig model) -> Maybe (ModelConfig model)
