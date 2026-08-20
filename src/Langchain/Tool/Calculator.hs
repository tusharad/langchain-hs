{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.Tool.Calculator
Description : Standard Calculator Tool implementation
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Calculator tool using Langchain.Core.Tool.
-}
module Langchain.Tool.Calculator
  ( calculatorTool
  , evaluateExpr
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T

import Langchain.Core.Error (toolError)
import Langchain.Core.Tool (Tool (..), createTool)

-- | Simple expression evaluator for arithmetic strings
evaluateExpr :: Text -> Either String Double
evaluateExpr txt =
  let cleanTxt = T.replace " " "" txt
   in case T.splitOn "+" cleanTxt of
        [a, b] -> case (reads (T.unpack a), reads (T.unpack b)) of
          ([(aNum, "")], [(bNum, "")]) -> Right (aNum + bNum)
          _ -> Left "Failed to parse numbers"
        _ -> case T.splitOn "*" cleanTxt of
          [a, b] -> case (reads (T.unpack a), reads (T.unpack b)) of
            ([(aNum, "")], [(bNum, "")]) -> Right (aNum * bNum)
            _ -> Left "Failed to parse numbers"
          _ -> Left "Unsupported expression format"

-- | Standard Calculator Tool instance
calculatorTool :: MonadIO m => Tool m
calculatorTool =
  createTool
    "calculator"
    "Useful for evaluating arithmetic math expressions like '2 + 2' or '3 * 4'"
    ( object
        [ "type" .= ("object" :: Text)
        , "properties"
            .= object
              [ "expression"
                  .= object
                    [ "type" .= ("string" :: Text)
                    , "description" .= ("Arithmetic expression string" :: Text)
                    ]
              ]
        , "required" .= (["expression"] :: [Text])
        ]
    )
    ( \case
        Object o -> case parseEither (.:? "expression") o of
          Right (Just expr) -> case evaluateExpr expr of
            Right num -> pure $ Right (T.pack $ show num)
            Left parseErr -> pure $ Left $ toolError (T.pack parseErr) (Just "calculator") Nothing
          _ -> pure $ Left $ toolError "Missing or invalid 'expression' field" (Just "calculator") Nothing
        _ -> pure $ Left $ toolError "Invalid arguments object" (Just "calculator") Nothing
    )
