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
Module      : Langchain.Tool.GenericSchema
Description : Type-safe tool parameter JSON schema derivation using GHC Generics
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Automatically derives OpenAI-compatible tool JSON schema objects from Haskell record types
using GHC Generics at compile time.
-}
module Langchain.Tool.GenericSchema
  ( DeriveToolSchema (..)
  , deriveToolParametersSchema
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as TS
import Data.Time (Day, UTCTime)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics

-- | Typeclass for deriving tool JSON schema parameters
class DeriveToolSchema a where
  deriveToolSchema :: Proxy a -> Value
  default deriveToolSchema :: (GToolRecordSchema (Rep a)) => Proxy a -> Value
  deriveToolSchema _ = deriveToolParametersSchema (Proxy :: Proxy a)

-- | Derive OpenAI tool parameter schema object
deriveToolParametersSchema ::
  forall a. (GToolRecordSchema (Rep a)) => Proxy a -> Value
deriveToolParametersSchema _ =
  let (props, reqs) = gToolRecordSchema (Proxy :: Proxy (Rep a))
   in object
        [ "type" .= ("object" :: Text)
        , "properties" .= object props
        , "required" .= reqs
        ]

class GToolRecordSchema (f :: Type -> Type) where
  gToolRecordSchema :: Proxy f -> ([(Key.Key, Value)], [Text])

instance (GToolRecordSchema f, GToolRecordSchema g) => GToolRecordSchema (f :*: g) where
  gToolRecordSchema _ =
    let (p1, r1) = gToolRecordSchema (Proxy :: Proxy f)
        (p2, r2) = gToolRecordSchema (Proxy :: Proxy g)
     in (p1 ++ p2, r1 ++ r2)

instance (GToolRecordSchema f) => GToolRecordSchema (M1 D c f) where
  gToolRecordSchema _ = gToolRecordSchema (Proxy :: Proxy f)

instance (GToolRecordSchema f) => GToolRecordSchema (M1 C c f) where
  gToolRecordSchema _ = gToolRecordSchema (Proxy :: Proxy f)

instance (Selector s, ToolFieldSchema a) => GToolRecordSchema (M1 S s (K1 R a)) where
  gToolRecordSchema _ =
    let selNameStr = selName (undefined :: M1 S s (K1 R a) p)
        propKey = Key.fromString selNameStr
        propSchema = toolFieldSchema (Proxy :: Proxy a)
        req = [TS.pack selNameStr | not (isOptionalField (Proxy :: Proxy a))]
     in ([(propKey, propSchema)], req)

class ToolFieldSchema a where
  toolFieldSchema :: Proxy a -> Value
  default toolFieldSchema :: (GToolRecordSchema (Rep a)) => Proxy a -> Value
  toolFieldSchema _ = deriveToolParametersSchema (Proxy :: Proxy a)

  isOptionalField :: Proxy a -> Bool
  isOptionalField _ = False

instance ToolFieldSchema Text where
  toolFieldSchema _ = object ["type" .= ("string" :: Text)]

instance ToolFieldSchema String where
  toolFieldSchema _ = object ["type" .= ("string" :: Text)]

instance ToolFieldSchema Char where
  toolFieldSchema _ = object ["type" .= ("string" :: Text)]

instance ToolFieldSchema Int where
  toolFieldSchema _ = object ["type" .= ("integer" :: Text)]

instance ToolFieldSchema Int8 where
  toolFieldSchema _ = object ["type" .= ("integer" :: Text)]

instance ToolFieldSchema Int16 where
  toolFieldSchema _ = object ["type" .= ("integer" :: Text)]

instance ToolFieldSchema Int32 where
  toolFieldSchema _ = object ["type" .= ("integer" :: Text)]

instance ToolFieldSchema Int64 where
  toolFieldSchema _ = object ["type" .= ("integer" :: Text)]

instance ToolFieldSchema Integer where
  toolFieldSchema _ = object ["type" .= ("integer" :: Text)]

instance ToolFieldSchema Word where
  toolFieldSchema _ = object ["type" .= ("integer" :: Text)]

instance ToolFieldSchema Word8 where
  toolFieldSchema _ = object ["type" .= ("integer" :: Text)]

instance ToolFieldSchema Word16 where
  toolFieldSchema _ = object ["type" .= ("integer" :: Text)]

instance ToolFieldSchema Word32 where
  toolFieldSchema _ = object ["type" .= ("integer" :: Text)]

instance ToolFieldSchema Word64 where
  toolFieldSchema _ = object ["type" .= ("integer" :: Text)]

instance ToolFieldSchema Double where
  toolFieldSchema _ = object ["type" .= ("number" :: Text)]

instance ToolFieldSchema Float where
  toolFieldSchema _ = object ["type" .= ("number" :: Text)]

instance ToolFieldSchema Scientific where
  toolFieldSchema _ = object ["type" .= ("number" :: Text)]

instance ToolFieldSchema Bool where
  toolFieldSchema _ = object ["type" .= ("boolean" :: Text)]

instance ToolFieldSchema UTCTime where
  toolFieldSchema _ =
    object
      [ "type" .= ("string" :: Text)
      , "format" .= ("date-time" :: Text)
      ]

instance ToolFieldSchema Day where
  toolFieldSchema _ =
    object
      [ "type" .= ("string" :: Text)
      , "format" .= ("date" :: Text)
      ]

instance ToolFieldSchema Value where
  toolFieldSchema _ = object ["type" .= ("object" :: Text)]

instance (ToolFieldSchema a) => ToolFieldSchema (Map.Map Text a) where
  toolFieldSchema _ =
    object
      [ "type" .= ("object" :: Text)
      , "additionalProperties" .= toolFieldSchema (Proxy :: Proxy a)
      ]

instance (ToolFieldSchema a) => ToolFieldSchema (Maybe a) where
  toolFieldSchema _ = toolFieldSchema (Proxy :: Proxy a)
  isOptionalField _ = True

instance {-# OVERLAPPABLE #-} (ToolFieldSchema a) => ToolFieldSchema [a] where
  toolFieldSchema _ =
    object
      [ "type" .= ("array" :: Text)
      , "items" .= toolFieldSchema (Proxy :: Proxy a)
      ]
