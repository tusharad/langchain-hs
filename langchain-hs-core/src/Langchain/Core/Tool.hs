{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.Core.Tool
Description : Effect-polymorphic Tool specification and parameter validation
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides effect-polymorphic 'Tool m' representation, schema generation, and argument parsing.
-}
module Langchain.Core.Tool
  ( Tool (..)
  , createTool
  , toolToValue
  ) where

import Data.Aeson
import Data.Text (Text)

import Langchain.Core.Error (LangchainError, toolError)

-- | Effect-polymorphic Tool abstraction
data Tool m = Tool
  { toolName :: Text
  -- ^ Unique identifier for the tool
  , toolDescription :: Text
  -- ^ Description explaining when and how to use the tool
  , toolSchema :: Value
  -- ^ JSON Schema describing expected parameters
  , toolExecute :: Value -> m (Either LangchainError Text)
  -- ^ Monadic execution function accepting JSON arguments and returning text output
  }

instance Show (Tool m) where
  show t = "Tool { toolName = " ++ show (toolName t) ++ " }"

-- | Helper to create a Tool from a name, description, schema, and execution function
createTool ::
  Text ->
  Text ->
  Value ->
  (Value -> m (Either LangchainError Text)) ->
  Tool m
createTool name desc schema execFn =
  Tool
    { toolName = name
    , toolDescription = desc
    , toolSchema = schema
    , toolExecute = execFn
    }

-- | Convert Tool definition to OpenAI/Ollama compatible function definition JSON object
toolToValue :: Tool m -> Value
toolToValue Tool {..} =
  object
    [ "type" .= ("function" :: Text)
    , "function"
        .= object
          [ "name" .= toolName
          , "description" .= toolDescription
          , "parameters" .= toolSchema
          ]
    ]
