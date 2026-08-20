# Retrieval-Augmented Generation (RAG) in LangChain-HS

Build complete, production-grade RAG pipelines in Haskell.

---

## 1. Document Loading and Recursive Splitting

```haskell
import Langchain.Prelude

-- Load and split documents
docs <- load (FileLoader "README.md")
let chunks = splitTextRecursive defaultRecursiveCharacterSplitterOps "Long text..."
```

## 2. Vector Stores & Embeddings

```haskell
-- In-memory vector store with Ollama embeddings
let embedder = OllamaEmbeddings "nomic-embed-text" "http://localhost:11434"
store <- fromDocuments docs embedder

-- Querying
matches <- similaritySearch store "How to install?" 3
```

## 3. Conversational QA with Question Reformulation

```haskell
mem <- newWindowBufferMemory 10 []
let qaChain = newConversationalRetrievalQA model (VectorStoreRetriever store 3) mem

res <- runConversationalRetrievalQA qaChain "What is the license?"
-- res contains qaAnswer, qaStandaloneQuestion, and qaSourceDocuments
```
