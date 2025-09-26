{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Langchain.Runnable.Chain
Description : Composition utilities for the Runnable typeclass
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer:  Tushar Adhatrao <tusharadhatrao@gmail.com>

This module provides various composition patterns for 'Runnable' instances,
allowing you to build complex processing pipelines from simpler components.

The primary abstractions include:

* 'RunnableSequence' - Chain multiple runnables sequentially
* 'RunnableBranch' - Select different processing branches based on input conditions
* 'RunnableMap' - Transform inputs or outputs when composing runnables

These abstractions follow functional programming patterns to create flexible
data processing pipelines for language model applications.
-}
module Langchain.Runnable.Chain
  ( -- * Core Data Types
    RunnableBranch (..)
  , RunnableMap (..)
  , RunnableSequence

    -- * Execution Functions
  , runBranch
  , runMap
  , runSequence

    -- * Composition Utilities
  , chain
  , branch
  , buildSequence
  , appendSequence
  , (|>>)
  ) where

import Data.List (find)
import Langchain.Runnable.Core

{- | Chains two 'Runnable' instances together sequentially.

The output of the first runnable is fed as input to the second.
If the first runnable fails, the error is returned immediately.

>>> :{
let textSplitter = TextSplitter defaultConfig
    llm = OpenAI defaultConfig
    summarizer input = chain textSplitter llm input
in summarizer "Split this text and then summarize each part."
:}
Right "The text was split into chunks and each part was summarized."
-}
chain ::
  (Runnable r1, Runnable r2, RunnableOutput r1 ~ RunnableInput r2) =>
  r1 ->
  r2 ->
  RunnableInput r1 ->
  IO (Either String (RunnableOutput r2))
chain r1 r2 input = do
  output1 <- invoke r1 input
  case output1 of
    Left err -> return $ Left err
    Right intermediate -> invoke r2 intermediate

{- | Creates a parallel composition of two 'Runnable' instances.

Both runnables receive the same input and their outputs are combined
into a tuple. If either runnable fails, the combined result fails.

>>> :{
let sentimentAnalyzer = LLMChain "Analyze sentiment of this text"
    keywordExtractor = LLMChain "Extract keywords from this text"
    analyzer text = branch sentimentAnalyzer keywordExtractor text
in analyzer "I love Haskell but monads can be challenging at first."
:}
Right ("Positive", ["Haskell", "love", "monads", "challenging"])
-}
branch ::
  (Runnable r1, Runnable r2, a ~ RunnableInput r1, a ~ RunnableInput r2) =>
  r1 ->
  r2 ->
  a ->
  IO (Either String (RunnableOutput r1, RunnableOutput r2))
branch r1 r2 input = do
  result1 <- invoke r1 input
  result2 <- invoke r2 input
  return $ (,) <$> result1 <*> result2

{- | A conditional branching structure for 'Runnable' instances.

'RunnableBranch' allows you to specify multiple condition-runnable pairs,
where the first runnable whose condition matches the input is invoked.
If no condition matches, a default runnable is used.

The conditions are functions that evaluate the input and return a boolean.
-}
data RunnableBranch a b
  = forall r.
    (Runnable r, RunnableInput r ~ a, RunnableOutput r ~ b) =>
    RunnableBranch [(a -> Bool, r)] r -- List of (condition, runnable) pairs and a default runnable

{- | Executes a 'RunnableBranch' by selecting the first matching runnable.

Evaluates each condition in order until one returns 'True', then invokes
the corresponding runnable. If no condition matches, invokes the default runnable.

>>> :{
let isShort text = length text < 100
    isQuestion text = last text == '?'
    shortTextHandler = LLMChain "Process short text"
    questionHandler = LLMChain "Answer the question"
    defaultHandler = LLMChain "Process general text"
    textProcessor = RunnableBranch [(isShort, shortTextHandler), (isQuestion, questionHandler)] defaultHandler
in runBranch textProcessor "How does this work?"
:}
Right "This is a question, so I'm handling it with the question processor."
-}
runBranch :: RunnableBranch a b -> a -> IO (Either String b)
runBranch (RunnableBranch options defaultR) input =
  case find (\(cond, _) -> cond input) options of
    Just (_, r) -> invoke r input
    Nothing -> invoke defaultR input

instance Runnable (RunnableBranch a b) where
  type RunnableInput (RunnableBranch a b) = a
  type RunnableOutput (RunnableBranch a b) = b

  invoke = runBranch

{- | A 'Runnable' that transforms input and/or output when executing another 'Runnable'.

'RunnableMap' allows you to adapt the input or output types of an existing 'Runnable'
to make it compatible with other components in your processing pipeline.
-}
data RunnableMap a b c
  = forall r.
    (Runnable r, RunnableInput r ~ b, RunnableOutput r ~ c) =>
    RunnableMap (a -> b) (c -> c) r -- input transform, output transform, and the runnable

{- | Executes a 'RunnableMap' by applying transformations to input and output.

First applies the input transformation function, then invokes the wrapped runnable,
and finally applies the output transformation function to the result (if successful).

>>> :{
let extractLength = length :: String -> Int
    isPalindrome str = str == reverse str
    lengthPalindrome = RunnableMap extractLength isPalindrome (pure True)
in runMap lengthPalindrome "hello"
:}
Right False
-}
runMap :: RunnableMap a b c -> a -> IO (Either String c)
runMap (RunnableMap inputFn outputFn r) input = do
  result <- invoke r (inputFn input)
  return $ fmap outputFn result

instance Runnable (RunnableMap a b c) where
  type RunnableInput (RunnableMap a b c) = a
  type RunnableOutput (RunnableMap a b c) = c

  invoke = runMap

{- | A sequence of 'Runnable' instances chained together.

'RunnableSequence' represents a pipeline where the output of each 'Runnable'
becomes the input to the next. This is the core abstraction for building
processing pipelines in Langchain.

The GADT construction ensures that the output type of each component
matches the input type of the next component.
-}
data RunnableSequence a b where
  RSNil :: RunnableSequence a a -- the empty chain, where the input and output types are the same.
  RSCons ::
    (Runnable r, RunnableInput r ~ a, RunnableOutput r ~ c) =>
    r ->
    RunnableSequence c b ->
    RunnableSequence a b -- RSCons adds a runnable at the front of the chain.

-- | Run a sequence of runnables, chaining the output of one as input to the next.
runSequence :: RunnableSequence a b -> RunnableInputHead a -> IO (Either String b)
runSequence RSNil input = return (Right input)
runSequence (RSCons r rs) input = do
  result <- invoke r input
  case result of
    Left err -> return (Left err)
    Right out -> runSequence rs out

instance Runnable (RunnableSequence a b) where
  type RunnableInput (RunnableSequence a b) = a
  type RunnableOutput (RunnableSequence a b) = b

  invoke = runSequence

-- | A type synonym to indicate the input type of the first runnable.
type RunnableInputHead a = a

{- | Builds a 'RunnableSequence' from two 'Runnable' instances.

This is a convenience function for creating a simple two-component sequence.

>>> :{
let parser = JSONParser defaultConfig
    validator = SchemaValidator personSchema
    personProcessor = buildSequence parser validator
in invoke personProcessor "{\"name\":\"John\",\"age\":30}"
:}
Right (Person "John" 30)
-}
buildSequence ::
  ( Runnable r1
  , Runnable r2
  , RunnableOutput r1 ~ RunnableInput r2
  ) =>
  r1 ->
  r2 ->
  RunnableSequence (RunnableInput r1) (RunnableOutput r2)
buildSequence r1 r2 = RSCons r1 (RSCons r2 RSNil)

{- | Appends a 'Runnable' to the end of a 'RunnableSequence'.

This allows you to incrementally build longer processing pipelines.

>>> :{
let retriever = DocumentRetriever defaultConfig
    llm = OpenAI defaultConfig
    formatter = OutputFormatter defaultConfig
    basePipeline = buildSequence retriever llm
    fullPipeline = appendSequence basePipeline formatter
in invoke fullPipeline "Tell me about Haskell's type system"
:}
Right "Haskell has a strong, static type system featuring type inference..."
-}
appendSequence ::
  ( Runnable r2
  , RunnableOutput (RunnableSequence a b) ~ RunnableInput r2
  ) =>
  RunnableSequence a b ->
  r2 ->
  RunnableSequence a (RunnableOutput r2)
appendSequence RSNil r = RSCons r RSNil
appendSequence (RSCons r1 rs) r2 = RSCons r1 (appendSequence rs r2)

{- | Operator version of 'chain' for more readable composition.

Allows for cleaner pipeline construction with an infix operator:

>>> textSplitter |>> embedder |>> retriever |>> llm $ "Explain monads in Haskell."
Right "Monads in Haskell are a design pattern that allows for sequencing computations..."
-}
(|>>) ::
  (Runnable r1, Runnable r2, RunnableOutput r1 ~ RunnableInput r2) =>
  r1 ->
  r2 ->
  RunnableInput r1 ->
  IO (Either String (RunnableOutput r2))
(|>>) = chain

infix 4 |>>
