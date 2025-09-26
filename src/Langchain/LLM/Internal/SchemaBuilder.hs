{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.LLM.Internal.SchemaBuilder
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
Description : DSL for constructing structured JSON Schemas for OpenAI's structured output API.

== Overview

This module defines a simple schema builder DSL for programmatically constructing
JSON Schemas compatible with the structured output features in the OpenAI API.

It supports nested objects, arrays, required fields, and custom types, and
provides infix operators for a fluent and expressive syntax.

== Example

@

let schema =
      emptyObject
        |+ ("name", JString)
        |+ ("age", JInteger)
        |++ ("address", buildSchema $
              emptyObject
                |+ ("city", JString)
                |+ ("zip", JInteger)
                |! "city"
            )
        |!! ["name", "age"]
        & buildSchema

printSchema schema
@
-}
module Langchain.LLM.Internal.SchemaBuilder
  ( -- * Core Types
    JsonType (..)
  , Property (..)
  , Schema (..)

    -- * Schema Construction
  , emptyObject
  , addProperty
  , addObjectProperty
  , requireField
  , requireFields
  , buildSchema

    -- * Schema Utilities
  , objectOf
  , arrayOf
  , toOpenAIFormat
  , printSchema

    -- * Infix Schema DSL
  , (|+)
  , (|++)
  , (|!)
  , (|!!)
  ) where

import Data.Aeson
import qualified Data.Map.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as T
import GHC.Generics

-- | Supported JSON types for schema generation.
data JsonType
  = JString
  | JNumber
  | JInteger
  | JBoolean
  | JNull
  | -- | Array of a specific type
    JArray JsonType
  | -- | Nested object schema
    JObject Schema
  deriving (Show, Eq, Generic)

instance ToJSON JsonType where
  toJSON JString = "string"
  toJSON JNumber = "number"
  toJSON JInteger = "integer"
  toJSON JBoolean = "boolean"
  toJSON JNull = "null"
  toJSON (JArray _) = "array"
  toJSON (JObject _) = "object"

-- | A named property with a given type (supports nested values).
newtype Property = Property JsonType
  deriving (Show, Eq, Generic)

instance ToJSON Property where
  toJSON (Property (JArray itemType)) =
    object ["type" .= ("array" :: Text), "items" .= Property itemType]
  toJSON (Property (JObject schema)) = toJSON schema
  toJSON (Property typ) = object ["type" .= typ]

-- | Complete schema representation.
data Schema = Schema
  { schemaProperties :: HM.Map Text Property
  , schemaRequired :: [Text]
  , schemaAdditionalProperties :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON Schema where
  toJSON (Schema props req add) =
    object
      [ "type" .= ("object" :: Text)
      , "properties" .= props
      , "required" .= req
      , "additionalProperties" .= add
      ]

-- | Internal builder for schema DSL.
newtype SchemaBuilder = SchemaBuilder Schema
  deriving (Show, Eq)

-- | Create an empty schema object.
emptyObject :: SchemaBuilder
emptyObject = SchemaBuilder $ Schema HM.empty [] False

-- | Add a simple field with a given name and type.
addProperty :: Text -> JsonType -> SchemaBuilder -> SchemaBuilder
addProperty name typ (SchemaBuilder s) =
  SchemaBuilder $
    s
      { schemaProperties =
          HM.insert name (Property typ) (schemaProperties s)
      }

-- | Add a nested object field with its own schema.
addObjectProperty :: Text -> Schema -> SchemaBuilder -> SchemaBuilder
addObjectProperty name nestedSchema (SchemaBuilder s) =
  SchemaBuilder $
    s
      { schemaProperties =
          HM.insert
            name
            (Property (JObject nestedSchema))
            (schemaProperties s)
      }

-- | Mark a field as required.
requireField :: Text -> SchemaBuilder -> SchemaBuilder
requireField name (SchemaBuilder s) =
  SchemaBuilder $ s {schemaRequired = name : schemaRequired s}

-- | Mark multiple fields as required.
requireFields :: [Text] -> SchemaBuilder -> SchemaBuilder
requireFields names builder = foldr requireField builder names

-- | Finalize the schema from a builder.
buildSchema :: SchemaBuilder -> Schema
buildSchema (SchemaBuilder s) = s

-- | Wrap a 'SchemaBuilder' as a nested object type.
objectOf :: SchemaBuilder -> JsonType
objectOf builder = JObject (buildSchema builder)

-- | Create an array of a given JSON type.
arrayOf :: JsonType -> JsonType
arrayOf = JArray

-- | Convert schema into a JSON 'Value' suitable for API submission.
toOpenAIFormat :: Schema -> Value
toOpenAIFormat = toJSON

-- | Pretty print a schema as formatted JSON.
printSchema :: Schema -> IO ()
printSchema = putStrLn . T.unpack . TL.toStrict . T.decodeUtf8 . encode

-- | Infix alias for 'addProperty'.
(|+) :: SchemaBuilder -> (Text, JsonType) -> SchemaBuilder
builder |+ (name, typ) = addProperty name typ builder

-- | Infix alias for 'addObjectProperty'.
(|++) :: SchemaBuilder -> (Text, Schema) -> SchemaBuilder
builder |++ (name, schema) = addObjectProperty name schema builder

-- | Infix alias for 'requireField'.
(|!) :: SchemaBuilder -> Text -> SchemaBuilder
builder |! name = requireField name builder

-- | Infix alias for 'requireFields'.
(|!!) :: SchemaBuilder -> [Text] -> SchemaBuilder
builder |!! names = requireFields names builder

infixl 7 |+, |++
infixl 6 |!, |!!
