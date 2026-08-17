# 🧠 Cortex-Agent

> **Cognitive Multi-Agent Deep Research & Enterprise Second-Brain Engine in Haskell**
>
> *Built on `langchain-hs`, `langchain-hs-core`, and `langchain-hs-graph` — combining the architectures of `gpt-researcher`, `Langchain-Chatchat`, `quivr`, and `langflow`.*

---

[![Build Status](https://img.shields.io/badge/tests-passed-brightgreen.svg)]()
[![Model Tested](https://img.shields.io/badge/Ollama-Qwen%203.5%209B%20%7C%20Llama%203.2-blue.svg)]()
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

---

## 🌟 Capabilities

### 1. 🔍 Autonomous Deep Research Engine (*Inspired by `gpt-researcher`*)
- **Chief Editor Orchestrator**: Plans research outlines, generates breadth/depth subtopic trees, and coordinates specialist sub-agents.
- **Concurrent Web Scraper & Context Curator**: Concurrent async HTTP fetching with TagSoup clean text extraction, rate-limiting, and word-budget context pruning (`MAX_CONTEXT_WORDS = 25000`).
- **Multi-Agent Writer & Fact-Checking Revision Loop**: `WriterAgent` drafts comprehensive sections with citations, while `FactCheckerAgent` extracts claims and verifies them against source evidence in a bounded revision loop.
- **Publication-Grade Reports**: Generates structured Markdown reports complete with executive summaries, table of contents, and a bibliography table with source word metrics.

### 2. 🗄️ Enterprise Knowledge Base & Hybrid RAG (*Inspired by `Langchain-Chatchat`*)
- **Multi-Tenant Brain Isolation**: Dedicated knowledge namespaces with independent SQLite storage, model parameters, and prompt configurations.
- **Chunk Header Enrichment**: Injects structured document metadata (`[Header: brain_id | source | chunk_index]`) into chunk text for higher retrieval precision.
- **Hybrid Sparse + Dense Retrieval**: Fuses BM25 inverted index keyword search with dense vector similarity via Reciprocal Rank Fusion (RRF).
- **LLM / Cross-Encoder Reranking**: Re-evaluates top candidate passages (0.0 to 10.0 score) before synthesis.

### 3. 🧩 Cognitive Task Decomposer & Router (*Inspired by `quivr`*)
- **`SplittedInput`**: Breaks down complex user requests and conversation history into explicit instructions, reasoning rationale, and atomic sub-tasks.
- **`TasksCompletion` Evaluator**: Evaluates whether tasks can be answered from current context or require external tool activation.
- **Dynamic System Prompt Rewriting**: Tailors transient system prompts to active tasks before multi-step synthesis.

### 4. ⚡ Declarative Dynamic Flow Runtime (*Inspired by `langflow`*)
- **JSON Flow Graph Interpreter**: Parses and executes arbitrary node graphs with cycle detection, topological ordering, and socket data routing.
- **Pre-Built Component Catalog**: Prompts, LLMs, Brain Retrievers, Web Scrapers, and Evaluators.
- **Real-Time Telemetry**: Server-Sent Events (SSE) and WebSocket protocol for live intermediate progress streaming.

---

## 🏗️ Architecture

```mermaid
flowchart TB
    subgraph UI ["Client & CLI"]
        CLI["cortex-cli"]
        SSE["SSE / WebSocket Stream"]
    end

    subgraph Cortex ["Cortex-Agent Core"]
        BR["Multi-Tenant Brain Store"]
        
        subgraph DeepResearch ["1. Deep Research Engine (gpt-researcher)"]
            PL["Outline Planner"]
            SC["Concurrent Scraper & Curator"]
            CO["Subtopic Conductor"]
            WR["Draft Writer Agent"]
            FC["Fact Checker Agent (Revision Loop)"]
            PB["Report Publisher"]
        end

        subgraph Knowledge ["2. Enterprise Knowledge Base (Chatchat)"]
            ING["Ingestion & Chunk Header Injector"]
            HYB["Hybrid Retriever (BM25 + Dense RRF)"]
            RER["LLM Cross-Encoder Reranker"]
        end

        subgraph Cognitive ["3. Cognitive Query Router (Quivr)"]
            DEC["Task Decomposer"]
            EVA["Completability Evaluator"]
            SYN["Dynamic Prompt Rewriter & Synthesizer"]
        end

        subgraph DynamicFlow ["4. Dynamic Flow Engine (Langflow)"]
            FLO["JSON Dynamic Flow Executor"]
            REG["Component Registry"]
        end
    end

    subgraph Backend ["LangChain Haskell Foundation"]
        CORE["langchain-hs-core (Pure AST & Streaming)"]
        GRAPH["langchain-hs-graph (StateGraph & Reducers)"]
        LLM["Ollama Provider (Qwen 3.5 9b / Llama 3.2)"]
    end

    UI --> Cortex
    Cortex --> Backend
```

---

## 🚀 CLI Quickstart

```bash
# Run autonomous deep research
cortex-cli research "Quantum Computing Algorithms"

# Create a new second-brain
cortex-cli brain create "Engineering Knowledge Base"
```
