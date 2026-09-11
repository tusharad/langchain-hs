/**
 * langchain-hs Documentation Client Logic
 */

document.addEventListener('DOMContentLoaded', () => {
  initThemeToggle();
  initProviderToggles();
  initCodeCopyButtons();
  initMobileMenu();
  initTableOfContents();
  initSearch();
  initLandingTabs();
  initMermaid();
});

/* ==========================================================================
   Theme Toggle (Dark / Light)
   ========================================================================== */
function initThemeToggle() {
  const themeToggleBtn = document.getElementById('theme-toggle');
  if (!themeToggleBtn) return;

  const currentTheme = localStorage.getItem('theme') || 
    (window.matchMedia('(prefers-color-scheme: light)').matches ? 'light' : 'dark');
  
  document.documentElement.setAttribute('data-theme', currentTheme);

  themeToggleBtn.addEventListener('click', () => {
    const isDark = document.documentElement.getAttribute('data-theme') !== 'light';
    const nextTheme = isDark ? 'light' : 'dark';
    document.documentElement.setAttribute('data-theme', nextTheme);
    localStorage.setItem('theme', nextTheme);
    
    // Refresh mermaid if present
    if (window.mermaid) {
      window.mermaid.initialize({
        startOnLoad: true,
        theme: nextTheme === 'light' ? 'default' : 'dark',
        themeVariables: {
          darkMode: nextTheme !== 'light',
          background: nextTheme === 'light' ? '#ffffff' : '#0e1322',
          primaryColor: '#6366f1',
          primaryTextColor: nextTheme === 'light' ? '#0f172a' : '#f1f5f9',
          primaryBorderColor: '#232d48',
          lineColor: '#06b6d4',
          secondaryColor: '#151d33',
          tertiaryColor: '#192237'
        }
      });
    }
  });
}

/* ==========================================================================
   Provider Toggles (Ollama vs OpenAI / OpenRouter)
   ========================================================================== */
function initProviderToggles() {
  let currentProvider = localStorage.getItem('langchain_provider') || 'ollama';

  const globalToggle = document.getElementById('global-provider-toggle');

  function updateGlobalToggleUI(provider) {
    if (!globalToggle) return;
    if (provider === 'openai') {
      globalToggle.innerHTML = `<span class="toggle-icon">⚡</span><span>OpenAI</span>`;
      globalToggle.classList.add('provider-openai');
      globalToggle.classList.remove('provider-ollama');
      globalToggle.title = 'Switch active provider to Ollama';
    } else {
      globalToggle.innerHTML = `<span class="toggle-icon">🦙</span><span>Ollama</span>`;
      globalToggle.classList.add('provider-ollama');
      globalToggle.classList.remove('provider-openai');
      globalToggle.title = 'Switch active provider to OpenAI';
    }
  }

  function setProvider(provider) {
    currentProvider = provider;
    localStorage.setItem('langchain_provider', provider);
    updateGlobalToggleUI(provider);

    document.querySelectorAll('.provider-group').forEach(group => {
      const panels = group.querySelectorAll('.provider-panel');
      const tabs = group.querySelectorAll('.provider-tab-btn');
      const commandPill = group.querySelector('.provider-command-pill code');

      let targetPanel = null;
      panels.forEach(p => {
        if (p.getAttribute('data-provider') === provider) {
          p.classList.add('active');
          targetPanel = p;
        } else {
          p.classList.remove('active');
        }
      });

      // Fallback if target provider doesn't exist in group
      if (!targetPanel && panels.length > 0) {
        panels[0].classList.add('active');
        targetPanel = panels[0];
      }

      tabs.forEach(t => {
        if (t.getAttribute('data-provider') === (targetPanel ? targetPanel.getAttribute('data-provider') : provider)) {
          t.classList.add('active');
        } else {
          t.classList.remove('active');
        }
      });

      if (commandPill && targetPanel) {
        const exe = targetPanel.getAttribute('data-exe') || '';
        commandPill.innerText = exe;
      }
    });
  }

  // Set up all provider groups on the page
  document.querySelectorAll('.provider-group').forEach(group => {
    // Avoid double header generation
    if (group.querySelector('.provider-tabs-header')) return;

    const panels = group.querySelectorAll('.provider-panel');
    if (panels.length === 0) return;

    const header = document.createElement('div');
    header.className = 'provider-tabs-header';

    const tabsContainer = document.createElement('div');
    tabsContainer.className = 'provider-tabs';

    panels.forEach(panel => {
      const pId = panel.getAttribute('data-provider') || 'ollama';
      const label = panel.getAttribute('data-label') || (pId === 'openai' ? '⚡ OpenAI / OpenRouter' : '🦙 Ollama (Local)');
      
      const tabBtn = document.createElement('button');
      tabBtn.className = 'provider-tab-btn' + (pId === currentProvider ? ' active' : '');
      tabBtn.setAttribute('data-provider', pId);
      tabBtn.innerText = label;
      tabBtn.addEventListener('click', () => {
        setProvider(pId);
      });
      tabsContainer.appendChild(tabBtn);
    });

    const actionsContainer = document.createElement('div');
    actionsContainer.className = 'provider-header-actions';

    const activePanel = group.querySelector(`.provider-panel[data-provider="${currentProvider}"]`) || panels[0];
    const initialExe = activePanel ? (activePanel.getAttribute('data-exe') || '') : '';

    const commandPill = document.createElement('div');
    commandPill.className = 'provider-command-pill';
    commandPill.title = 'Click to copy execution command';
    commandPill.innerHTML = `<span class="pill-label">Run:</span> <code>${initialExe}</code>`;
    commandPill.addEventListener('click', async () => {
      const curActive = group.querySelector('.provider-panel.active') || panels[0];
      const exe = curActive ? curActive.getAttribute('data-exe') : '';
      if (exe) {
        try {
          await navigator.clipboard.writeText(exe);
          const oldLabel = commandPill.querySelector('.pill-label').innerText;
          commandPill.querySelector('.pill-label').innerText = 'Copied!';
          setTimeout(() => {
            commandPill.querySelector('.pill-label').innerText = oldLabel;
          }, 1800);
        } catch (e) {
          console.error(e);
        }
      }
    });

    actionsContainer.appendChild(commandPill);
    header.appendChild(tabsContainer);
    header.appendChild(actionsContainer);

    group.insertBefore(header, group.firstChild);
  });

  // Global toggle click handler
  if (globalToggle) {
    globalToggle.addEventListener('click', () => {
      const nextProvider = currentProvider === 'ollama' ? 'openai' : 'ollama';
      setProvider(nextProvider);
    });
  }

  // Initial activation
  setProvider(currentProvider);
}

/* ==========================================================================
   Code Snippet Copy Buttons
   ========================================================================== */
function initCodeCopyButtons() {
  const codeBlocks = document.querySelectorAll('div.sourceCode, pre');

  codeBlocks.forEach((block) => {
    // Avoid double wrapping
    if (block.closest('.code-block-wrapper')) return;

    const pre = block.tagName === 'PRE' ? block : block.querySelector('pre');
    if (!pre) return;

    const code = pre.querySelector('code') || pre;
    const textContent = code.innerText;

    // Detect language from class
    let lang = 'code';
    const classes = (block.className + ' ' + (code.className || '')).split(/\s+/);
    for (const cls of classes) {
      if (cls.startsWith('language-') || cls.startsWith('sourceCode')) {
        const extracted = cls.replace('language-', '').replace('sourceCode', '').trim();
        if (extracted && extracted !== '') {
          lang = extracted;
          break;
        }
      }
    }

    const wrapper = document.createElement('div');
    wrapper.className = 'code-block-wrapper';

    const header = document.createElement('div');
    header.className = 'code-header';

    const langSpan = document.createElement('span');
    langSpan.className = 'code-lang';
    langSpan.innerHTML = `<svg width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><polyline points="16 18 22 12 16 6"></polyline><polyline points="8 6 2 12 8 18"></polyline></svg> ${lang}`;

    const copyBtn = document.createElement('button');
    copyBtn.className = 'btn-copy';
    copyBtn.innerHTML = `<svg width="13" height="13" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><rect x="9" y="9" width="13" height="13" rx="2" ry="2"></rect><path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"></path></svg> Copy`;

    copyBtn.addEventListener('click', async () => {
      try {
        await navigator.clipboard.writeText(textContent);
        copyBtn.classList.add('copied');
        copyBtn.innerHTML = `<svg width="13" height="13" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><polyline points="20 6 9 17 4 12"></polyline></svg> Copied!`;
        setTimeout(() => {
          copyBtn.classList.remove('copied');
          copyBtn.innerHTML = `<svg width="13" height="13" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><rect x="9" y="9" width="13" height="13" rx="2" ry="2"></rect><path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"></path></svg> Copy`;
        }, 2000);
      } catch (err) {
        console.error('Failed to copy', err);
      }
    });

    header.appendChild(langSpan);
    header.appendChild(copyBtn);

    block.parentNode.insertBefore(wrapper, block);
    wrapper.appendChild(header);
    wrapper.appendChild(block);
  });
}

/* ==========================================================================
   Mobile Sidebar Menu
   ========================================================================== */
function initMobileMenu() {
  const toggleBtn = document.getElementById('mobile-toggle');
  const sidebar = document.querySelector('.doc-sidebar');

  if (toggleBtn && sidebar) {
    toggleBtn.addEventListener('click', () => {
      sidebar.classList.toggle('open');
    });

    // Close when clicking outside on mobile
    document.addEventListener('click', (e) => {
      if (sidebar.classList.contains('open') && !sidebar.contains(e.target) && !toggleBtn.contains(e.target)) {
        sidebar.classList.remove('open');
      }
    });
  }
}

/* ==========================================================================
   Table of Contents Scrollspy
   ========================================================================== */
function initTableOfContents() {
  const headers = document.querySelectorAll('.doc-content h2, .doc-content h3');
  const tocContainer = document.querySelector('.toc-list');
  if (!tocContainer || headers.length === 0) return;

  tocContainer.innerHTML = '';

  headers.forEach((header, index) => {
    if (!header.id) {
      header.id = 'heading-' + index + '-' + header.innerText.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/(^-|-$)/g, '');
    }

    const li = document.createElement('li');
    const a = document.createElement('a');
    a.href = '#' + header.id;
    a.className = 'toc-link' + (header.tagName === 'H3' ? ' nested' : '');
    a.innerText = header.innerText;
    li.appendChild(a);
    tocContainer.appendChild(li);
  });

  // IntersectionObserver for active header
  const observer = new IntersectionObserver((entries) => {
    entries.forEach((entry) => {
      if (entry.isIntersecting) {
        const id = entry.target.id;
        document.querySelectorAll('.toc-link').forEach((link) => {
          if (link.getAttribute('href') === '#' + id) {
            link.classList.add('active');
          } else {
            link.classList.remove('active');
          }
        });
      }
    });
  }, { rootMargin: '0px 0px -70% 0px' });

  headers.forEach((h) => observer.observe(h));
}

/* ==========================================================================
   Instant Search Modal (Cmd+K / Ctrl+K)
   ========================================================================== */
const searchDocs = [
  // 20 Components
  { title: "Chat Models", category: "Components", url: "/components/chat-models.html", snippet: "Simple chat invocation with ChatOllama and ChatOpenAI." },
  { title: "Streaming", category: "Components", url: "/components/streaming.html", snippet: "Real-time token streaming with streamChat and StreamEvents." },
  { title: "Langchain Monad", category: "Components", url: "/components/monad.html", snippet: "Monadic LLM computation with LangchainT, ReaderT, and Exception handling." },
  { title: "Tools & Function Calling", category: "Components", url: "/components/tools.html", snippet: "Defining typed Haskell tools with FunctionDefinition and tool invocation." },
  { title: "Structured Outputs", category: "Components", url: "/components/structured-output.html", snippet: "Generating typed JSON schemas and Aeson FromJSON decoding." },
  { title: "RAG & Embeddings", category: "Components", url: "/components/rag.html", snippet: "Vector embeddings, cosine similarity, Document chunking and retrieval." },
  { title: "Retrievers & Ensembles", category: "Components", url: "/components/retrievers.html", snippet: "BM25 keyword search, Vector store retriever, and ensemble rank fusion." },
  { title: "Memory Systems", category: "Components", url: "/components/memory.html", snippet: "ConversationBufferMemory and conversational context tracking." },
  { title: "Retrieval QA Chains", category: "Components", url: "/components/retrieval-qa.html", snippet: "End-to-end question answering over indexed knowledge bases." },
  { title: "Map-Reduce Document Processing", category: "Components", url: "/components/map-reduce.html", snippet: "Hierarchical summarization and chunk reduction over large corpora." },
  { title: "ReAct Agent", category: "Components", url: "/components/react-agent.html", snippet: "Reasoning and acting loop with tool inspection and step execution." },
  { title: "Plan-and-Execute Agent", category: "Components", url: "/components/plan-and-execute.html", snippet: "Multi-step planner with dynamic task decomposition and re-planning." },
  { title: "Guardrails & Safety", category: "Components", url: "/components/guardrails.html", snippet: "Input/output moderation, PII redaction, and semantic guardrail enforcement." },
  { title: "Resilience & Circuit Breakers", category: "Components", url: "/components/resilience.html", snippet: "Retry policies, exponential backoff, rate limiting, and fallback providers." },
  { title: "Observability & Tracing", category: "Components", url: "/components/observability.html", snippet: "Callbacks, OpenTelemetry tracing, and execution timing metrics." },
  { title: "Model Context Protocol (MCP)", category: "Components", url: "/components/mcp.html", snippet: "Stdio and HTTP MCP client connecting external tool servers." },
  { title: "Stateful Graphs (StateGraph)", category: "Components", url: "/components/state-graph.html", snippet: "Cyclic workflows, typed channels, state reducers, and conditional branching." },
  { title: "Multi-Agent Systems", category: "Components", url: "/components/multi-agent.html", snippet: "Supervisor routing, consensus debate, and multi-agent coordination." },
  { title: "Human-in-the-Loop (HITL)", category: "Components", url: "/components/hitl.html", snippet: "Interrupting graph workflows for approval, review, and resuming state." },
  { title: "Runnables & AST Composition", category: "Components", url: "/components/runnables.html", snippet: "Pipe (|>>), parallel (&>&), fallback, and AST pipeline execution." },

  // Getting Started & Concepts
  { title: "Installation & Packages", category: "Getting Started", url: "/getting-started/installation.html", snippet: "Stack, Cabal, Nix setup, GHC 9.6+, monorepo architecture." },
  { title: "Quickstart (5-Minute Guide)", category: "Getting Started", url: "/getting-started/quickstart.html", snippet: "Building your first LLM app with Ollama, OpenAI, or Gemini." },
  { title: "Building Your First Agent", category: "Getting Started", url: "/getting-started/first-agent.html", snippet: "Constructing ReAct and Plan-and-Execute agents with typed tools." },
  { title: "Stateful Graphs (StateGraph)", category: "Getting Started", url: "/getting-started/first-graph.html", snippet: "Creating cyclic state machines, reducers, and graph compilation." },
  { title: "Monorepo Architecture", category: "Core Concepts", url: "/concepts/architecture.html", snippet: "Zero-dependency pure core, graph engine, and high-level ecosystem." },
  { title: "Runnable AST Pipelines", category: "Core Concepts", url: "/concepts/runnables.html", snippet: "RunnableTree GADT, |>>, &>&, >>># operators, and AST interpretation." },
  { title: "Models & Multi-Modal Messages", category: "Core Concepts", url: "/concepts/models-messages.html", snippet: "ChatModel, ContentBlock (text, image, tool use), and StreamEvents." },
  { title: "Tools & Model Context Protocol (MCP)", category: "Core Concepts", url: "/concepts/tools-mcp.html", snippet: "Effect-polymorphic tools, stdio & HTTP MCP client integration." },
  { title: "State Graphs & Checkpointers", category: "Core Concepts", url: "/concepts/state-graphs.html", snippet: "StateGraph s m, TVar memory checkpointers, SQLite persistence, time-travel." },
  { title: "Multi-Agent Architectures", category: "Core Concepts", url: "/concepts/multi-agent.html", snippet: "Supervisor teams, multi-agent debate, majority voting, and shared blackboards." },
  { title: "RAG & Hybrid Retrieval", category: "Core Concepts", url: "/concepts/rag-retrieval.html", snippet: "Vector stores, BM25 keyword search, Reciprocal Rank Fusion (RRF), Rerankers." },
  { title: "Memory Systems & Caching", category: "Core Concepts", url: "/concepts/memory-caching.html", snippet: "WindowBufferMemory, SummaryMemory, EntityMemory, SQLite caching backend." },
  { title: "Observability & Resilience", category: "Core Concepts", url: "/concepts/observability-resilience.html", snippet: "OpenTelemetry spans, structured logs, CircuitBreakers, and rate limiters." },
  { title: "Local AI with Ollama", category: "Guides", url: "/guides/local-models-ollama.html", snippet: "Connecting DeepSeek-R1, Llama 3, Qwen, and nomic-embed-text locally." },
  { title: "Human-in-the-Loop (HITL)", category: "Guides", url: "/guides/human-in-the-loop.html", snippet: "Interrupting graph execution for human approval and resuming with SQLite." },
  { title: "Structured Type Extraction", category: "Guides", url: "/guides/structured-outputs.html", snippet: "Extracting typed Haskell data structures and JSON schemas safely." },
  { title: "Building an MCP Agent", category: "Guides", url: "/guides/building-mcp-agent.html", snippet: "Connecting Haskell agents to GitHub, filesystem, and SQLite MCP servers." },
  { title: "API Cheat Sheet", category: "Reference", url: "/api/cheat-sheet.html", snippet: "Comprehensive type, operator, and reducer reference for Langchain.Prelude." },
  { title: "Monorepo Packages", category: "Reference", url: "/api/packages.html", snippet: "Package versions, dependencies, and Hackage documentation." }
];

function initSearch() {
  const searchBtn = document.getElementById('search-btn');
  const searchModal = document.getElementById('search-modal');
  const searchInput = document.getElementById('search-input');
  const searchResults = document.getElementById('search-results');

  if (!searchBtn || !searchModal || !searchInput || !searchResults) return;

  function openSearch() {
    searchModal.classList.add('open');
    searchInput.value = '';
    renderResults(searchDocs.slice(0, 5));
    searchInput.focus();
  }

  function closeSearch() {
    searchModal.classList.remove('open');
  }

  searchBtn.addEventListener('click', openSearch);

  searchModal.addEventListener('click', (e) => {
    if (e.target === searchModal) closeSearch();
  });

  window.addEventListener('keydown', (e) => {
    if ((e.metaKey || e.ctrlKey) && e.key.toLowerCase() === 'k') {
      e.preventDefault();
      if (searchModal.classList.contains('open')) {
        closeSearch();
      } else {
        openSearch();
      }
    }
    if (e.key === 'Escape' && searchModal.classList.contains('open')) {
      closeSearch();
    }
  });

  searchInput.addEventListener('input', (e) => {
    const q = e.target.value.toLowerCase().trim();
    if (!q) {
      renderResults(searchDocs.slice(0, 5));
      return;
    }
    const filtered = searchDocs.filter(d => 
      d.title.toLowerCase().includes(q) || 
      d.category.toLowerCase().includes(q) || 
      d.snippet.toLowerCase().includes(q)
    );
    renderResults(filtered);
  });

  function renderResults(items) {
    searchResults.innerHTML = '';
    if (items.length === 0) {
      searchResults.innerHTML = `<div style="padding: 1.5rem; text-align: center; color: var(--text-dim);">No matching documentation found.</div>`;
      return;
    }

    items.forEach((item) => {
      const a = document.createElement('a');
      a.href = item.url;
      a.className = 'search-result-item';
      a.innerHTML = `
        <div class="search-result-category">${item.category}</div>
        <div class="search-result-title">${item.title}</div>
        <div class="search-result-snippet">${item.snippet}</div>
      `;
      searchResults.appendChild(a);
    });
  }
}

/* ==========================================================================
   Landing Page Interactive Tabs
   ========================================================================== */
function initLandingTabs() {
  const tabBtns = document.querySelectorAll('.tab-btn');
  const tabPanes = document.querySelectorAll('.tab-pane');

  tabBtns.forEach((btn) => {
    btn.addEventListener('click', () => {
      const target = btn.getAttribute('data-tab');

      tabBtns.forEach((b) => b.classList.remove('active'));
      tabPanes.forEach((p) => p.classList.remove('active'));

      btn.classList.add('active');
      const pane = document.getElementById('tab-' + target);
      if (pane) pane.classList.add('active');
    });
  });
}

/* ==========================================================================
   Mermaid.js Initialization
   ========================================================================== */
function initMermaid() {
  const mermaidBlocks = document.querySelectorAll('pre.mermaid, div.mermaid');
  if (mermaidBlocks.length === 0) return;

  const currentTheme = document.documentElement.getAttribute('data-theme') || 'dark';

  // Load Mermaid dynamically if not loaded
  if (!window.mermaid) {
    const script = document.createElement('script');
    script.src = 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js';
    script.onload = () => {
      window.mermaid.initialize({
        startOnLoad: true,
        theme: currentTheme === 'light' ? 'default' : 'dark',
        themeVariables: {
          darkMode: currentTheme !== 'light',
          background: currentTheme === 'light' ? '#ffffff' : '#0e1322',
          primaryColor: '#6366f1',
          primaryTextColor: currentTheme === 'light' ? '#0f172a' : '#f1f5f9',
          primaryBorderColor: '#232d48',
          lineColor: '#06b6d4',
          secondaryColor: '#151d33',
          tertiaryColor: '#192237'
        }
      });
      window.mermaid.run();
    };
    document.head.appendChild(script);
  }
}
