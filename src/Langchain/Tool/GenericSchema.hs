{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
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
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as TS
import GHC.Generics

-- | Typeclass for deriving tool JSON schema parameters
class DeriveToolSchema a where
  deriveToolSchema :: Proxy a -> Value
  default deriveToolSchema :: (Generic a, GToolRecordSchema (Rep a)) => Proxy a -> Value
  deriveToolSchema _ = deriveToolParametersSchema (Proxy :: Proxy a)

-- | Derive OpenAI tool parameter schema object
deriveToolParametersSchema
  :: forall a. (Generic a, GToolRecordSchema (Rep a)) => Proxy a -> Value
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
        req = if isOptionalField (Proxy :: Proxy a) then [] else [TS.pack selNameStr]
     in ([(propKey, propSchema)], req)

class ToolFieldSchema a where
  toolFieldSchema :: Proxy a -> Value
  isOptionalField :: Proxy a -> Bool
  isOptionalField _ = False

instance ToolFieldSchema Text where
  toolFieldSchema _ = object ["type" .= ("string" :: Text)]

instance ToolFieldSchema Int where
  toolFieldSchema _ = object ["type" .= ("integer" :: Text)]

instance ToolFieldSchema Double where
  toolFieldSchema _ = object ["type" .= ("number" :: Text)]

instance ToolFieldSchema Float where
  toolFieldSchema _ = object ["type" .= ("number" :: Text)]

instance ToolFieldSchema Bool where
  toolFieldSchema _ = object ["type" .= ("boolean" :: Text)]

instance (ToolFieldSchema a) => ToolFieldSchema (Maybe a) where
  toolFieldSchema _ = toolFieldSchema (Proxy :: Proxy a)
  isOptionalField _ = True

instance {-# OVERLAPPABLE #-} (ToolFieldSchema a) => ToolFieldSchema [a] where
  toolFieldSchema _ =
    object
      [ "type" .= ("array" :: Text)
      , "items" .= toolFieldSchema (Proxy :: Proxy a)
      ]
