/**
 * langchain-hs Documentation Client Logic
 * Modern, zero-emoji, responsive interface
 */

document.addEventListener('DOMContentLoaded', () => {
  initThemeToggle();
  initProviderToggles();
  initCodeCopyButtons();
  initMobileMenu();
  initTableOfContents();
  initSearch();
  initLandingTabs();
  initCopyInstallButtons();
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
  });
}

/* ==========================================================================
   Provider Toggles (Ollama vs OpenAI / OpenRouter vs Gemini)
   ========================================================================== */
function initProviderToggles() {
  let currentProvider = localStorage.getItem('langchain_provider') || 'ollama';
  const globalToggle = document.getElementById('global-provider-toggle');

  function updateGlobalToggleUI(provider) {
    if (!globalToggle) return;
    const btns = globalToggle.querySelectorAll('.provider-switch-btn');
    btns.forEach(btn => {
      if (btn.getAttribute('data-provider') === provider) {
        btn.classList.add('active');
      } else {
        btn.classList.remove('active');
      }
    });
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
        const activeTarget = targetPanel ? targetPanel.getAttribute('data-provider') : provider;
        if (t.getAttribute('data-provider') === activeTarget) {
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
    if (group.querySelector('.provider-tabs-header')) return;

    const panels = group.querySelectorAll('.provider-panel');
    if (panels.length === 0) return;

    const header = document.createElement('div');
    header.className = 'provider-tabs-header';

    const tabsContainer = document.createElement('div');
    tabsContainer.className = 'provider-tabs';

    panels.forEach(panel => {
      const pId = panel.getAttribute('data-provider') || 'ollama';
      let label = panel.getAttribute('data-label');
      if (!label) {
        label = pId === 'gemini' ? 'Gemini' : (pId === 'openai' ? 'OpenAI / OpenRouter' : 'Ollama (Local)');
      }
      
      const tabBtn = document.createElement('button');
      tabBtn.className = 'provider-tab-btn' + (pId === currentProvider ? ' active' : '');
      tabBtn.setAttribute('data-provider', pId);
      
      const dot = document.createElement('span');
      dot.className = `pill-dot pill-dot-${pId}`;
      const text = document.createElement('span');
      text.innerText = label;
      
      tabBtn.appendChild(dot);
      tabBtn.appendChild(text);
      tabBtn.addEventListener('click', () => {
        setProvider(pId);
      });
      tabsContainer.appendChild(tabBtn);
    });

    const actionsContainer = document.createElement('div');
    actionsContainer.className = 'provider-actions';

    const activePanel = group.querySelector(`.provider-panel[data-provider="${currentProvider}"]`) || panels[0];
    const initialExe = activePanel ? (activePanel.getAttribute('data-exe') || '') : '';

    const commandPill = document.createElement('div');
    commandPill.className = 'provider-command-pill';
    commandPill.title = 'Click to copy run command';
    commandPill.innerHTML = `<span>Run:</span> <code>${initialExe}</code>`;
    commandPill.addEventListener('click', async () => {
      const curActive = group.querySelector('.provider-panel.active') || panels[0];
      const exe = curActive ? curActive.getAttribute('data-exe') : '';
      if (exe) {
        try {
          await navigator.clipboard.writeText(exe);
          const orig = commandPill.innerHTML;
          commandPill.innerHTML = `<span>Copied!</span> <code>${exe}</code>`;
          setTimeout(() => { commandPill.innerHTML = orig; }, 1800);
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
    const btns = globalToggle.querySelectorAll('.provider-switch-btn');
    btns.forEach(btn => {
      btn.addEventListener('click', (e) => {
        e.stopPropagation();
        const p = btn.getAttribute('data-provider');
        if (p) setProvider(p);
      });
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
    // Avoid double wrapping and skip mermaid diagrams or terminal outputs
    if (block.closest('.code-block-wrapper')) return;
    if (block.classList.contains('mermaid') || block.classList.contains('terminal-body')) return;
    if (block.closest('.terminal-output-wrapper')) return;

    const pre = block.tagName === 'PRE' ? block : block.querySelector('pre');
    if (!pre) return;
    if (pre.classList.contains('mermaid') || pre.classList.contains('terminal-body')) return;

    const code = pre.querySelector('code') || pre;
    const textContent = code.innerText;

    // Detect language from class
    let lang = 'haskell';
    const classes = (block.className + ' ' + (code.className || '') + ' ' + (pre.className || '')).split(/\s+/);
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
   Quick Install Copy Buttons
   ========================================================================== */
function initCopyInstallButtons() {
  document.querySelectorAll('.quick-install-box').forEach(box => {
    box.style.cursor = 'pointer';
    box.title = 'Click to copy install command';
    box.addEventListener('click', async () => {
      const text = box.querySelector('.install-cmd')?.innerText || box.innerText;
      try {
        await navigator.clipboard.writeText(text);
        const originalContent = box.innerHTML;
        box.innerHTML = `<span style="color:#10b981;font-weight:600;">Copied to clipboard!</span>`;
        setTimeout(() => { box.innerHTML = originalContent; }, 1800);
      } catch (e) {
        console.error(e);
      }
    });
  });
}

/* ==========================================================================
   Mobile Sidebar & Menu Toggle
   ========================================================================== */
function initMobileMenu() {
  const toggleBtn = document.getElementById('mobile-toggle');
  const sidebar = document.querySelector('.doc-sidebar');
  const navCenter = document.getElementById('main-nav');

  if (toggleBtn) {
    toggleBtn.addEventListener('click', () => {
      if (sidebar) {
        sidebar.classList.toggle('open');
      } else if (navCenter) {
        navCenter.classList.toggle('open-mobile');
      }
    });

    document.addEventListener('click', (e) => {
      if (sidebar && sidebar.classList.contains('open') && !sidebar.contains(e.target) && !toggleBtn.contains(e.target)) {
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
   Instant Search Modal (Ctrl+K / Cmd+K)
   ========================================================================== */
const searchDocs = [
  { title: "Chat Models", category: "Components", url: "/components/chat-models.html", snippet: "Simple chat invocation with ChatOllama, ChatOpenAI, and Gemini." },
  { title: "Streaming", category: "Components", url: "/components/streaming.html", snippet: "Real-time incremental token streaming and Conduit pipelines." },
  { title: "Langchain Monad", category: "Components", url: "/components/monad.html", snippet: "Monadic LLM computation with LangchainT, ReaderT, and error handling." },
  { title: "Tools & Function Calling", category: "Components", url: "/components/tools.html", snippet: "Defining typed Haskell tools with FunctionDefinition and execution." },
  { title: "Structured Outputs", category: "Components", url: "/components/structured-output.html", snippet: "Guaranteed JSON schema generation parsed into type-safe Aeson records." },
  { title: "RAG & Embeddings", category: "Components", url: "/components/rag.html", snippet: "Vector embeddings, cosine distance, Document chunking and semantic search." },
  { title: "Retrievers & Ensembles", category: "Components", url: "/components/retrievers.html", snippet: "BM25 keyword search, vector stores, and Reciprocal Rank Fusion (RRF)." },
  { title: "Memory Systems", category: "Components", url: "/components/memory.html", snippet: "ConversationBufferMemory and persistent multi-turn history tracking." },
  { title: "Retrieval QA Chains", category: "Components", url: "/components/retrieval-qa.html", snippet: "End-to-end question answering pipeline synthesizing relevant context." },
  { title: "ReAct Agent", category: "Components", url: "/components/react-agent.html", snippet: "Autonomous reasoning and acting loop with tool selection and observations." },
  { title: "Plan-and-Execute Agent", category: "Components", url: "/components/plan-and-execute.html", snippet: "Decoupled macro-planner creating structured task DAGs with execution." },
  { title: "Guardrails & Safety", category: "Components", url: "/components/guardrails.html", snippet: "Pre- and post-execution filtering, PII redaction, and policy moderation." },
  { title: "Resilience & Circuit Breakers", category: "Components", url: "/components/resilience.html", snippet: "Exponential backoff retries, fallback providers, and circuit breakers." },
  { title: "Observability & Tracing", category: "Components", url: "/components/observability.html", snippet: "Callbacks, execution timing, token metrics, and OpenTelemetry spans." },
  { title: "Model Context Protocol (MCP)", category: "Components", url: "/components/mcp.html", snippet: "Stdio and HTTP JSON-RPC client connecting external tool servers." },
  { title: "Stateful Graphs (StateGraph)", category: "Components", url: "/components/state-graph.html", snippet: "Cyclic workflows, pure state reducers, and checkpointed state machines." },
  { title: "Multi-Agent Systems", category: "Components", url: "/components/multi-agent.html", snippet: "Supervisor routing, consensus debate, and multi-agent coordination." },
  { title: "Human-in-the-Loop (HITL)", category: "Components", url: "/components/hitl.html", snippet: "Interrupt execution graphs for human approval, edit state, and resume." },
  { title: "Runnables & AST Composition", category: "Components", url: "/components/runnables.html", snippet: "First-class AST composition via pipe (|>>), parallel (&>&), and fallback." },
  { title: "Installation", category: "Getting Started", url: "/getting-started/installation.html", snippet: "Stack, Cabal setup, GHC 9.8+, package installation from Hackage." },
  { title: "Quickstart", category: "Getting Started", url: "/getting-started/quickstart.html", snippet: "Building your first LLM app with Ollama, OpenAI, or Gemini in 5 minutes." },
  { title: "First ReAct Agent", category: "Getting Started", url: "/getting-started/first-agent.html", snippet: "Constructing autonomous ReAct agents with typed tools." },
  { title: "First StateGraph", category: "Getting Started", url: "/getting-started/first-graph.html", snippet: "Creating cyclic state machines, pure reducers, and graph compilation." },
  { title: "Prelude Cheat Sheet", category: "Reference", url: "/api/cheat-sheet.html", snippet: "Comprehensive reference for Langchain.Prelude types, operators, and reducers." },
  { title: "Hackage Docs", category: "Reference", url: "https://hackage.haskell.org/package/langchain-hs", snippet: "Official Hackage documentation and Haddock API signatures." }
];

function initSearch() {
  const searchBtn = document.getElementById('search-btn');
  const searchModal = document.getElementById('search-modal');
  const searchInput = document.getElementById('search-input');
  const searchResults = document.getElementById('search-results');

  if (!searchBtn || !searchModal || !searchInput || !searchResults) return;

  function renderResults(items) {
    searchResults.innerHTML = '';
    if (items.length === 0) {
      searchResults.innerHTML = '<div style="padding: 1rem; color: var(--text-dim); text-align: center;">No documentation found</div>';
      return;
    }
    items.forEach((item, idx) => {
      const a = document.createElement('a');
      a.className = 'search-result-item' + (idx === 0 ? ' selected' : '');
      a.href = item.url;
      a.innerHTML = `
        <div class="search-result-title">${item.title} <span class="tag is-small" style="background:var(--palette-1);color:#fff;margin-left:0.5rem;">${item.category}</span></div>
        <div class="search-result-snippet">${item.snippet}</div>
      `;
      searchResults.appendChild(a);
    });
  }

  function openSearch() {
    searchModal.classList.add('open');
    searchInput.value = '';
    renderResults(searchDocs.slice(0, 6));
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
      renderResults(searchDocs.slice(0, 6));
      return;
    }
    const filtered = searchDocs.filter(d => 
      d.title.toLowerCase().includes(q) || 
      d.category.toLowerCase().includes(q) || 
      d.snippet.toLowerCase().includes(q)
    );
    renderResults(filtered);
  });
}

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
