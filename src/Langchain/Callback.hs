{- |
Module:      Langchain.Callback
Copyright:   (c) 2025 Tushar Adhatrao
License:     MIT
Maintainer:  Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability:   experimental

This module provides a callback system for Langchain's language model operations.
Callbacks allow users to perform actions at different stages of an LLM operation,
such as when it starts, completes, or encounters an error. This is useful for
logging, monitoring, or integrating with external systems.

The callback system is inspired by the Langchain Python library's callback
functionality: [Langchain Callbacks](https://python.langchain.com/docs/concepts/callbacks/).

== Examples

See the documentation for 'stdOutCallback' for a basic example, or check the
examples for 'generate', 'chat', and 'stream' in the 'Langchain.LLM.Ollama' module
for practical usage in LLM operations.
-}
module Langchain.Callback
  ( -- * Event Types
    Event (..)

    -- * Callback Interface
  , Callback

    -- * Standard Implementations
  , stdOutCallback
  ) where

{- | Represents different events that can occur during a language model operation.
These events can be used to trigger callbacks at various stages.
-}
data Event
  = -- | Indicates the start of an LLM operation, such as generating text or chatting.
    LLMStart
  | -- | Indicates the successful completion of an LLM operation.
    LLMEnd
  | -- | Indicates an error occurred during the LLM operation, with the error message.
    LLMError String
  deriving (Show, Eq)

{- | A callback is a function that takes an 'Event' and performs some IO action.
This allows users to react to different stages of LLM operations, such as logging
or updating a UI.

=== Examples

To create a custom callback that logs events to a file:

@
import System.IO
myCallback :: Callback
myCallback event = do
  handle <- openFile "llm_log.txt" AppendMode
  case event of
    LLMStart -> hPutStrLn handle "LLM operation started"
    LLMEnd -> hPutStrLn handle "LLM operation completed"
    LLMError err -> hPutStrLn handle $ "LLM error: " ++ err
  hClose handle
@
-}
type Callback = Event -> IO ()

{- | A standard callback that prints event messages to the standard output.
This is useful for simple debugging or monitoring of LLM operations.

=== Examples

Using 'stdOutCallback' in an LLM operation:

@
let callbacks = [stdOutCallback]
result <- generate (Ollama "llama3.2:latest" callbacks) "What is 2+2?" Nothing
-- Output will include:
-- Model operation started
-- Model completed with
-- (depending on success or error)
@
-}
stdOutCallback :: Callback
stdOutCallback event = case event of
  LLMStart -> putStrLn "Model operation started"
  LLMEnd -> putStrLn "Model completed with"
  LLMError err -> putStrLn $ "Error occurred: " ++ err
