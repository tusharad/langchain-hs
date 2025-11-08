---
sidebar_position: 2
---

# Document Loaders

The `DocumentLoader` provides a unified interface for loading `Document` Types from various sources.

## Document

```haskell
data Document = Document
  { pageContent :: Text
  , metadata :: Map Text Value
  }
```

The `Document` type is a simple data structure that contains two fields:
- `pageContent`: A `Text` field that contains the content of the document.
- `metadata`: A `Map` that contains metadata about the document. The keys are of type `Text`, and the values are of type `Value` (from the Aeson library).

Document as Monoid instance, so two documents can be appended together.

## BaseLoader

```haskell
class BaseLoader m where
  load :: m -> IO (Either String [Document])
  loadAndSplit :: m -> IO (Either String [Text])
```

The `BaseLoader` typeclass defines two methods:
- `load`: This method takes a `BaseLoader` instance and returns an `IO` action that produces either an error message or a list of `Document`s.
- `loadAndSplit`: This method takes a `BaseLoader` instance and returns an `IO` action that produces either an error message or a list of `Text` chunks. This is useful for splitting the document into smaller pieces.

## Integrations 

Right now, langchain-hs provides below integrations, with more integrations planned in the roadmap:

- `FileLoader`: Loads documents from a file path.
- `PdfLoader`: Loads documents from a PDF file path.
- `DirectoryLoader`: Loads documents from a directory. It can recursively load files from subdirectories and filter files based on their extensions.

**DirectoryLoaderOptions**

```haskell
data DirectoryLoaderOptions = DirectoryLoaderOptions
  { recursiveDepth :: Maybe Int
  , extensions :: [String]
  , excludeHidden :: Bool
  , useMultithreading :: Bool
  }
```

The `DirectoryLoaderOptions` type is a data structure that contains options for loading documents from a directory. It has the following fields:
- `recursiveDepth`: An optional `Int` that specifies the maximum depth of recursion when loading files from subdirectories. If `Nothing`, it will load files from all subdirectories.
- `extensions`: A list of `String` that specifies the file extensions to include when loading files. If empty, it will load all files.
- `excludeHidden`: A `Bool` that specifies whether to exclude hidden files (files starting with a dot) when loading files. The default is `True`.
- `useMultithreading`: A `Bool` that specifies whether to use multithreading when loading files. The default is `False`.

`defaultDirectoryLoaderOptions` is also provided.

## Examples 

- FileLoader

```haskell
{-# LANGUAGE OverloadedStrings #-}

module LangchainLib (runApp) where

import Langchain.DocumentLoader.FileLoader
import Langchain.DocumentLoader.Core

runApp :: IO ()
runApp = do
    let loader = FileLoader "/home/user/haskell/sample-proj/README.md"
    docs <- load loader
    chunks <- loadAndSplit loader
    print docs
    print chunks
```

- PdfLoader 

```haskell
{-# LANGUAGE OverloadedStrings #-}

module LangchainLib (runApp) where

import Langchain.DocumentLoader.Core
import Langchain.DocumentLoader.PdfLoader 

runApp :: IO ()
runApp = do
    let loader = PdfLoader  "/home/user/Documents/TS/langchain/SOP.pdf"
    docs <- load loader
    chunks <- loadAndSplit loader
    print docs
    print chunks
```

- DirectoryLoader

```haskell
{-# LANGUAGE OverloadedStrings #-}

module LangchainLib (runApp) where

import Langchain.DocumentLoader.Core
import Langchain.DocumentLoader.DirectoryLoader

runApp :: IO ()
runApp = do
    let loader = DirectoryLoader { 
        dirPath = "/home/user/Documents/TS/langchain"
      , directoryLoaderOptions = defaultDirectoryLoaderOptions {
          recursiveDepth = Just 2
      }
    }
    docs <- load loader
    chunks <- loadAndSplit loader
    print docs
    print chunks
```

These documents are useful to embed into vector store and build RAG tools.
