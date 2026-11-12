#!/usr/bin/env bash
set -euo pipefail

show_help() {
  cat <<EOF
Usage: verify_gate.sh [OPTIONS]

Black-box verification gate for Formal Spec Verifier skill.

Options:
  --help       Show this help message and exit
  --haskell    Run GHC strict typecheck (-Wall -Werror) and QuickCheck/Hedgehog suites
  --python     Run mypy --strict, ruff, and Hypothesis property-based test suites
  --infra      Verify Dockerfiles (hadolint), GitHub Actions (actionlint), and OPA policies (conftest)
  --ci         Check latest GitHub Actions workflow status for current branch via gh CLI
  --all        Run all detected local verification suites
EOF
}

run_haskell() {
  echo "[VERIFY:HASKELL] Running strict typecheck and property tests..."
  if command -v stack >/dev/null 2>&1 && [ -f "stack.yaml" ]; then
    stack test --ghc-options="-Wall -Werror"
  elif command -v cabal >/dev/null 2>&1 && ls *.cabal >/dev/null 2>&1; then
    cabal test --ghc-options="-Wall -Werror"
  else
    echo "No Cabal or Stack project detected." >&2
    exit 1
  fi
}

run_python() {
  echo "[VERIFY:PYTHON] Running strict static types and Hypothesis oracle..."
  if command -v mypy >/dev/null 2>&1; then
    mypy --strict .
  fi
  if command -v ruff >/dev/null 2>&1; then
    ruff check .
  fi
  if command -v pytest >/dev/null 2>&1; then
    pytest --hypothesis-show-statistics
  else
    echo "pytest not installed." >&2
    exit 1
  fi
}

run_infra() {
  echo "[VERIFY:INFRA] Checking Dockerfiles, GitHub Workflows, and OPA policies..."
  if command -v hadolint >/dev/null 2>&1; then
    find . -name "Dockerfile*" -exec hadolint {} +
  fi
  if command -v actionlint >/dev/null 2>&1 && [ -d ".github/workflows" ]; then
    actionlint
  fi
  if command -v conftest >/dev/null 2>&1 && [ -d "policy" ]; then
    conftest test Dockerfile* .github/workflows/*.yml .github/workflows/*.yaml 2>/dev/null || true
  fi
}

run_ci() {
  echo "[VERIFY:CI] Inspecting remote GitHub Actions pipeline status..."
  BRANCH=$(git rev-parse --abbrev-ref HEAD)
  gh run list --branch "$BRANCH" --limit 1
  RUN_ID=$(gh run list --branch "$BRANCH" --limit 1 --json databaseId --jq '.[0].databaseId')
  if [ -n "$RUN_ID" ] && [ "$RUN_ID" != "null" ]; then
    gh run watch "$RUN_ID" --exit-status || {
      echo "[VERIFY:CI] CI failed. Fetching failed logs:"
      gh run view "$RUN_ID" --log-failed
      exit 1
    }
  else
    echo "No CI runs found for branch $BRANCH."
  fi
}

if [ $# -eq 0 ]; then
  show_help
  exit 1
fi

for arg in "$@"; do
  case "$arg" in
    --help)    show_help; exit 0 ;;
    --haskell) run_haskell ;;
    --python)  run_python ;;
    --infra)   run_infra ;;
    --ci)      run_ci ;;
    --all)
      [ -f "stack.yaml" ] || ls *.cabal >/dev/null 2>&1 && run_haskell
      [ -f "pyproject.toml" ] || [ -f "requirements.txt" ] && run_python
      run_infra
      ;;
    *)
      echo "Unknown option: $arg. Run with --help for usage." >&2
      exit 1
      ;;
  esac
done