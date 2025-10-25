{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Langchain.Tool.Calculator
Description : Mathematical expression calculator tool for LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

This module provides a calculator tool that can be used with LangChain agents to perform
arithmetic operations. It parses and evaluates mathematical expressions including:

* Basic arithmetic: addition (+), subtraction (-), multiplication (*), division (/)
* Exponentiation (^)
* Parentheses for grouping
* Floating-point numbers

The calculator uses a parser combinator approach to handle operator precedence correctly.

Example usage:

@
import Langchain.Tool.Calculator
import Langchain.Tool.Core (runTool)

main :: IO ()
main = do
  let calc = CalculatorTool
  result <- runTool calc "2 + 3 * 4"
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right value -> putStrLn $ "Result: " ++ show value
  -- Output: Result: 14.0
@
-}
module Langchain.Tool.Calculator
  ( CalculatorTool (..)
  , Expr (..)
  , parseExpression
  , evaluateExpression
  ) where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Tool.Core (Tool (..))
import Text.ParserCombinators.Parsec

-- | Expression data type for our calculator
data Expr
  = Number_ Double
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  deriving (Show, Eq)

-- | Calculator Tool implementation
data CalculatorTool = CalculatorTool
  deriving (Show)

instance Tool CalculatorTool where
  type Input CalculatorTool = Text
  type Output CalculatorTool = Either String Double

  toolName _ = "calculator"

  toolDescription _ =
    "A calculator tool that can perform basic arithmetic operations. "
      <> "Input should be a mathematical expression like '2 + 3 * 4'."

  runTool _ input = do
    case parseExpression input of
      Left err -> return $ Left $ "Failed to parse expression: " ++ show err
      Right expr -> return $ Right $ evaluateExpression expr

-- | Parse a mathematical expression from Text
parseExpression :: Text -> Either ParseError Expr
parseExpression = parse expr "" . T.unpack
  where
    expr = addSubExpr

    addSubExpr = do
      left <- mulDivExpr
      rest left
      where
        rest left =
          ( do
              void $ char '+' <* spaces
              right <- mulDivExpr
              rest (Add left right)
          )
            <|> ( do
                    void $ char '-' <* spaces
                    right <- mulDivExpr
                    rest (Sub left right)
                )
            <|> return left

    mulDivExpr = do
      left <- powExpr
      rest left
      where
        rest left =
          ( do
              void $ char '*' <* spaces
              right <- powExpr
              rest (Mul left right)
          )
            <|> ( do
                    void $ char '/' <* spaces
                    right <- powExpr
                    rest (Div left right)
                )
            <|> return left

    powExpr = do
      left <- factor
      rest left
      where
        rest left =
          ( do
              void $ char '^' <* spaces
              right <- factor
              rest (Pow left right)
          )
            <|> return left

    factor =
      (Number_ . read <$> numberStr)
        <|> (spaces *> char '(' *> spaces *> expr <* spaces <* char ')' <* spaces)

    numberStr = do
      i <- many1 digit
      d <- option "" $ (:) <$> char '.' <*> many1 digit
      spaces
      return (i ++ d)

-- | Evaluate a parsed expression to a Double
evaluateExpression :: Expr -> Double
evaluateExpression expr = case expr of
  Number_ n -> n
  Add a b -> evaluateExpression a + evaluateExpression b
  Sub a b -> evaluateExpression a - evaluateExpression b
  Mul a b -> evaluateExpression a * evaluateExpression b
  Div a b -> evaluateExpression a / evaluateExpression b
  Pow a b -> evaluateExpression a ** evaluateExpression b
