{- |
Module      : Langchain.DocumentLoader.Core
Description : Core document loading functionality for LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Implementation of LangChain's document loading abstraction, providing:

- Document representation with content and metadata
- Typeclass for loading/splitting documents from various sources
- Integration with text splitting capabilities

For more information on document loader in the original Langchain library, see:
https://python.langchain.com/docs/concepts/document_loaders/

Example usage:

@
-- Create a document
doc :: Document
doc = Document "Sample content" (fromList [("source", String "example.txt")])

-- Hypothetical file loader instance
data FileLoader = FileLoader FilePath

instance BaseLoader FileLoader where
  load (FileLoader path) = do
    content <- readFile path
    return $ Right [Document content (fromList [("source", String (T.pack path))])]
@

Test case patterns:

>>> mempty :: Document
Document {pageContent = "", metadata = fromList []}

>>> doc1 = Document "Hello" (fromList [("a", Number 1)])
>>> doc2 = Document " World" (fromList [("b", Bool True)])
>>> doc1 <> doc2
Document {pageContent = "Hello World", metadata = fromList [("a", Number 1), ("b", Bool True)]}
-}
module Langchain.DocumentLoader.Core
  ( -- * Document Representation
    Document (..)

    -- * Loading Interface
  , BaseLoader (..)
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Map (Map, empty)
import Data.Text.Lazy (Text)
import Langchain.Error (LangchainResult)

{- | Document container with content and metadata.
Used for storing loaded data and associated metadata like source URLs or page numbers.

Example:

>>> Document "Hello World" (fromList [("source", String "example.txt")])
Document {pageContent = "Hello World", metadata = fromList [("source",String "example.txt")]}
-}
data Document = Document
  { pageContent :: Text
  -- ^ The text content of the document
  , metadata :: Map Text Value
  -- ^ Additional metadata (e.g., source, page number)
  }
  deriving (Show, Eq)

{- | Semigroup instance combines both content and metadata

>>> let doc1 = Document "A" (fromList [("x", Number 1)])
>>> let doc2 = Document "B" (fromList [("y", Bool True)])
>>> doc1 <> doc2
Document {pageContent = "AB", metadata = fromList [("x", Number 1), ("y", Bool True)]}
-}
instance Semigroup Document where
  doc1 <> doc2 =
    Document
      (pageContent doc1 <> pageContent doc2)
      (metadata doc1 <> metadata doc2)

{- | Monoid instance provides empty document:

>>> mempty :: Document
Document {pageContent = "", metadata = fromList []}
-}
instance Monoid Document where
  mempty = Document mempty empty

{- | Typeclass for document loading implementations.
Implementations should define how to:

1. Load full documents with 'load'
2. Load and split content with 'loadAndSplit'

Example instance for text files:

@
instance BaseLoader FilePath where
  load path = do
    content <- readFile path
    return $ Right [Document content (fromList [("source", String (T.pack path))])]

  loadAndSplit path = do
    content <- readFile path
    return $ Right (splitText defaultCharacterSplitterOps content)
@
-}
class BaseLoader loader where
  -- | Load all documents from the source.
  load :: loader -> IO (LangchainResult [Document])

  loadM :: MonadIO m => loader -> m (LangchainResult [Document])
  loadM loader = liftIO $ load loader

  -- | Load all the document and split them using recursiveCharacterSpliter
  loadAndSplit :: loader -> IO (LangchainResult [Text])

  loadAndSplitM :: MonadIO m => loader -> m (LangchainResult [Text])
  loadAndSplitM loader = liftIO $ loadAndSplit loader

{- $examples
Key test case demonstrations:

1. Metadata merging
   >>> let doc1 = Document "A" (fromList [("x", Number 1)])
   >>> let doc2 = Document "B" (fromList [("y", Bool True)])
   >>> metadata (doc1 <> doc2)
   fromList [("x", Number 1), ("y", Bool True)]

2. File loading error handling
   >>> load (FileLoader "non-existent.txt")
   Left "File not found: non-existent.txt"

3. Content splitting
   >>> loadAndSplit (FileLoader "test.txt")
   Right ["Paragraph 1", "Paragraph 2"]
-}

--  TODO: Implement lazy versions of Document and load.
-- Lazily load documents from the source.
-- lazyLoad :: m -> IO (Either String [Document])
