{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Langchain.OutputParser.Structured
Description : Type-safe structured output extraction using GHC Generics and JSON Schemas
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Generates JSON Schemas automatically from Haskell types using GHC Generics,
prompts the ChatModel for structured JSON output, and parses the response into typed values
with an automatic error-correction retry loop.
-}
module Langchain.OutputParser.Structured
  ( StructuredOutput (..)
  , TypeSchema (..)
  , GRecordSchema (..)
  , genericJsonSchema
  , toOllamaSchema
  , fromOllamaSchema
  , structuredInvoke
  , structuredInvokeWithRetries
  , extractJsonFromMarkdown
  ) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, Value (..), decode, encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TE
import Data.Time (Day, UTCTime)
import qualified Data.Vector as V
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics

import Langchain.Core.Error (LangchainError, parsingError)
import Langchain.Core.Model
  ( ChatModel (..)
  , Message (..)
  , extractMessageText
  , systemMessage
  , userMessage
  )
import qualified Ollama.Types.Format.SchemaBuilder as SB

-- | Typeclass for types that declare a JSON Schema and structured parser
class (FromJSON a) => StructuredOutput a where
  outputSchema :: Proxy a -> Value
  default outputSchema :: (GRecordSchema (Rep a)) => Proxy a -> Value
  outputSchema _ = genericJsonSchema (Proxy :: Proxy a)

-- | Generic JSON Schema derivation helper
genericJsonSchema :: forall a. (GRecordSchema (Rep a)) => Proxy a -> Value
genericJsonSchema _ =
  let (props, reqs) = gRecordSchema (Proxy :: Proxy (Rep a))
   in object
        [ "type" .= ("object" :: Text)
        , "properties" .= object props
        , "required" .= reqs
        ]

class GRecordSchema (f :: Type -> Type) where
  gRecordSchema :: Proxy f -> ([(Key.Key, Value)], [Text])

instance (GRecordSchema f, GRecordSchema g) => GRecordSchema (f :*: g) where
  gRecordSchema _ =
    let (p1, r1) = gRecordSchema (Proxy :: Proxy f)
        (p2, r2) = gRecordSchema (Proxy :: Proxy g)
     in (p1 ++ p2, r1 ++ r2)

instance (GRecordSchema f) => GRecordSchema (M1 D c f) where
  gRecordSchema _ = gRecordSchema (Proxy :: Proxy f)

instance (GRecordSchema f) => GRecordSchema (M1 C c f) where
  gRecordSchema _ = gRecordSchema (Proxy :: Proxy f)

instance (Selector s, TypeSchema a) => GRecordSchema (M1 S s (K1 R a)) where
  gRecordSchema _ =
    let selNameStr = selName (undefined :: M1 S s (K1 R a) p)
        propKey = Key.fromString selNameStr
        propSchema = typeJsonSchema (Proxy :: Proxy a)
        req = [TS.pack selNameStr | not (isOptionalType (Proxy :: Proxy a))]
     in ([(propKey, propSchema)], req)

-- | Typeclass defining JSON Schema mapping for Haskell primitive and composite types
class TypeSchema a where
  typeJsonSchema :: Proxy a -> Value
  default typeJsonSchema :: (GRecordSchema (Rep a)) => Proxy a -> Value
  typeJsonSchema _ = genericJsonSchema (Proxy :: Proxy a)

  isOptionalType :: Proxy a -> Bool
  isOptionalType _ = False

instance (TypeSchema a) => TypeSchema (Maybe a) where
  typeJsonSchema _ = typeJsonSchema (Proxy :: Proxy a)
  isOptionalType _ = True

instance TypeSchema Text where
  typeJsonSchema _ = object ["type" .= ("string" :: Text)]

instance TypeSchema String where
  typeJsonSchema _ = object ["type" .= ("string" :: Text)]

instance TypeSchema Char where
  typeJsonSchema _ = object ["type" .= ("string" :: Text)]

instance TypeSchema Int where
  typeJsonSchema _ = object ["type" .= ("integer" :: Text)]

instance TypeSchema Int8 where
  typeJsonSchema _ = object ["type" .= ("integer" :: Text)]

instance TypeSchema Int16 where
  typeJsonSchema _ = object ["type" .= ("integer" :: Text)]

instance TypeSchema Int32 where
  typeJsonSchema _ = object ["type" .= ("integer" :: Text)]

instance TypeSchema Int64 where
  typeJsonSchema _ = object ["type" .= ("integer" :: Text)]

instance TypeSchema Integer where
  typeJsonSchema _ = object ["type" .= ("integer" :: Text)]

instance TypeSchema Word where
  typeJsonSchema _ = object ["type" .= ("integer" :: Text)]

instance TypeSchema Word8 where
  typeJsonSchema _ = object ["type" .= ("integer" :: Text)]

instance TypeSchema Word16 where
  typeJsonSchema _ = object ["type" .= ("integer" :: Text)]

instance TypeSchema Word32 where
  typeJsonSchema _ = object ["type" .= ("integer" :: Text)]

instance TypeSchema Word64 where
  typeJsonSchema _ = object ["type" .= ("integer" :: Text)]

instance TypeSchema Double where
  typeJsonSchema _ = object ["type" .= ("number" :: Text)]

instance TypeSchema Float where
  typeJsonSchema _ = object ["type" .= ("number" :: Text)]

instance TypeSchema Scientific where
  typeJsonSchema _ = object ["type" .= ("number" :: Text)]

instance TypeSchema Bool where
  typeJsonSchema _ = object ["type" .= ("boolean" :: Text)]

instance TypeSchema UTCTime where
  typeJsonSchema _ =
    object
      [ "type" .= ("string" :: Text)
      , "format" .= ("date-time" :: Text)
      ]

instance TypeSchema Day where
  typeJsonSchema _ =
    object
      [ "type" .= ("string" :: Text)
      , "format" .= ("date" :: Text)
      ]

instance TypeSchema Value where
  typeJsonSchema _ = object ["type" .= ("object" :: Text)]

instance (TypeSchema a) => TypeSchema (Map.Map Text a) where
  typeJsonSchema _ =
    object
      [ "type" .= ("object" :: Text)
      , "additionalProperties" .= typeJsonSchema (Proxy :: Proxy a)
      ]

instance {-# OVERLAPPABLE #-} (TypeSchema a) => TypeSchema [a] where
  typeJsonSchema _ =
    object
      [ "type" .= ("array" :: Text)
      , "items" .= typeJsonSchema (Proxy :: Proxy a)
      ]

-- | Convert a Langchain JSON Schema Value into an ollama-haskell Schema
toOllamaSchema :: Value -> Maybe SB.Schema
toOllamaSchema (Object obj) = do
  propsVal <- KM.lookup "properties" obj
  propsMap <- case propsVal of
    Object pObj ->
      Just $
        Map.fromList
          [ (Key.toText k, SB.Property jt)
          | (k, v) <- KM.toList pObj
          , Just jt <- [valueToJsonType v]
          ]
    _ -> Nothing
  let reqs = case KM.lookup "required" obj of
        Just (Array arr) -> [t | String t <- V.toList arr]
        _ -> []
  pure $ SB.Schema propsMap reqs
  where
    valueToJsonType :: Value -> Maybe SB.JsonType
    valueToJsonType (Object vObj) = case KM.lookup "type" vObj of
      Just (String "string") -> Just SB.JString
      Just (String "integer") -> Just SB.JInteger
      Just (String "number") -> Just SB.JNumber
      Just (String "boolean") -> Just SB.JBoolean
      Just (String "null") -> Just SB.JNull
      Just (String "array") -> do
        itemVal <- KM.lookup "items" vObj
        itemType <- valueToJsonType itemVal
        pure $ SB.JArray itemType
      Just (String "object") -> do
        subSchema <- toOllamaSchema (Object vObj)
        pure $ SB.JObject subSchema
      _ -> Nothing
    valueToJsonType _ = Nothing
toOllamaSchema _ = Nothing

-- | Convert an ollama-haskell Schema into a Langchain JSON Schema Value
fromOllamaSchema :: SB.Schema -> Value
fromOllamaSchema (SB.Schema props reqs) =
  object
    [ "type" .= ("object" :: Text)
    , "properties"
        .= object [Key.fromText k .= jsonTypeToValue jt | (k, SB.Property jt) <- Map.toList props]
    , "required" .= reqs
    ]
  where
    jsonTypeToValue :: SB.JsonType -> Value
    jsonTypeToValue SB.JString = object ["type" .= ("string" :: Text)]
    jsonTypeToValue SB.JInteger = object ["type" .= ("integer" :: Text)]
    jsonTypeToValue SB.JNumber = object ["type" .= ("number" :: Text)]
    jsonTypeToValue SB.JBoolean = object ["type" .= ("boolean" :: Text)]
    jsonTypeToValue SB.JNull = object ["type" .= ("null" :: Text)]
    jsonTypeToValue (SB.JArray jt) =
      object
        [ "type" .= ("array" :: Text)
        , "items" .= jsonTypeToValue jt
        ]
    jsonTypeToValue (SB.JObject subSchema) = fromOllamaSchema subSchema

-- | Invoke a ChatModel and extract a typed StructuredOutput value
structuredInvoke ::
  forall a model m.
  (StructuredOutput a, ChatModel model, MonadIO m, MonadError LangchainError m) =>
  model ->
  [Message] ->
  m a
structuredInvoke model msgs = structuredInvokeWithRetries model msgs 3

-- | Invoke a ChatModel with up to N retry iterations with error-correction feedback
structuredInvokeWithRetries ::
  forall a model m.
  (StructuredOutput a, ChatModel model, MonadIO m, MonadError LangchainError m) =>
  model ->
  [Message] ->
  Int ->
  m a
structuredInvokeWithRetries model baseMsgs maxAttempts = do
  let schema = outputSchema (Proxy :: Proxy a)
      schemaStr = TE.decodeUtf8 $ LBSC.toStrict $ encode schema
      systemInstruction =
        systemMessage
          ( "You are a structured data extractor. You must respond ONLY with a valid JSON object matching this JSON Schema:\n"
              <> schemaStr
              <> "\nDo NOT wrap the JSON in Markdown backticks or provide conversational text."
          )
      fullConversation = systemInstruction : baseMsgs
  go fullConversation maxAttempts
  where
    go conv attemptsLeft = do
      resp <- invoke model conv Nothing
      let rawText = extractMessageText resp
          cleanJson = extractJsonFromMarkdown rawText
          bs = LBSC.fromStrict (TE.encodeUtf8 cleanJson)
      case decode bs of
        Just parsedVal -> pure parsedVal
        Nothing ->
          if attemptsLeft <= 1
            then
              throwError $
                parsingError
                  ( "Failed to parse structured JSON output from LLM: "
                      <> rawText
                      <> " (Schema: "
                      <> TE.decodeUtf8 (LBSC.toStrict (encode (outputSchema (Proxy :: Proxy a))))
                      <> ")"
                  )
                  (Just "structuredInvoke")
                  Nothing
            else do
              let correctionMsg =
                    userMessage
                      ( "Your previous response was not valid JSON matching the schema. Error: failed to parse.\n"
                          <> "Please re-output ONLY valid JSON matching the schema."
                      )
                  updatedConv = conv ++ [resp, correctionMsg]
              go updatedConv (attemptsLeft - 1)

-- | Robust helper to unwrap JSON from markdown ```json ``` blocks
extractJsonFromMarkdown :: Text -> Text
extractJsonFromMarkdown t =
  let stripped = TS.strip t
   in if "```json" `TS.isPrefixOf` stripped
        then
          let afterPrefix = TS.drop 7 stripped
           in case TS.breakOn "```" afterPrefix of
                (jsonPart, _) -> TS.strip jsonPart
        else
          if "```" `TS.isPrefixOf` stripped
            then
              let afterPrefix = TS.drop 3 stripped
               in case TS.breakOn "```" afterPrefix of
                    (jsonPart, _) -> TS.strip jsonPart
            else stripped
