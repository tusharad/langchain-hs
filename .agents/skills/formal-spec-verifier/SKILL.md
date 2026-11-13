---
name: formal-spec-verifier
description: Enforces lightweight formal verification, Design by Contract, and stateful property-based testing before writing or modifying code. Use when implementing, refactoring, or fixing bugs in backend APIs, database models, S3 storage interactions, cron jobs, Haskell modules, Python services, Dockerfiles, or GitHub Actions CI/CD pipelines.
---

# Formal Specification & Verification Skill

You are operating under a strict **Spec-Driven Lightweight Formal Methods** protocol. Your goal is to eliminate requirement ambiguity, prevent defensive code bloat, and verify system invariants across stateless logic, stateful cloud boundaries (DB, S3, queues), and CI/CD infrastructure.

## Decision Tree: Selecting the Verification Strategy

Before writing any specification or test, classify the user's target change using this decision tree:

1. **What layer of the stack is being modified?**
   - **Pure Domain / Data Transformation** → Enforce **Refinement Types / Smart Constructors** + **Algebraic Property Tests** (round-trip, commutativity, invariant preservation).
   - **API Endpoint / Cron Job / Webhook** → Enforce **Idempotency Laws** (`f(f(s, x), x) == f(s, x)`), **Frame Conditions** (what DB tables/S3 keys must NOT change), and **OpenAPI Wire Compatibility**.
   - **Database + External Storage (Postgres + S3 / Redis)** → Enforce **Stateful Model-Based Testing** (compare real/mocked IO execution traces against a pure in-memory `Map` state machine) + **Compensation/Rollback Laws**.
   - **Database Schema Migration** → Enforce **Expand-Contract Backwards Compatibility** (Migration `N+1` must pass the test suite of Code Version `N`).
   - **Dockerfile / GitHub Actions / Infrastructure** → Enforce **Static Policy Invariants** via `scripts/verify_gate.sh --infra`.

2. **Is this a Bug Fix or Edge-Case Addition?**
   - **YES** → Apply the **Rewrite-Over-Patch Rule**:
     1. Reproduce the bug as a failing property or state-machine transition in the test suite first.
     2. Refactor the core data type or state transition so the edge case is handled by construction.
     3. **FORBIDDEN**: Adding ad-hoc defensive `if/else` patches at the bottom of existing functions that increase cyclomatic complexity.
   - **NO (New Feature)** → Proceed through Phases 1 to 4 sequentially.

## Execution Protocol

### Phase 1: Contract Elicitation & Ambiguity Halt
1. Read `resources/system-law-template.yaml` to structure your analysis.
2. Output a filled **System Law Specification** covering:
   - `REQUIRES` (Preconditions & boundary parsing)
   - `ENSURES` (Postconditions)
   - `LAWS` (Idempotency, atomicity/compensation between DB and S3, conservation invariants)
   - `MODIFIES_ONLY` (Frame conditions: explicit list of mutable tables, buckets, and files)
   - `COMPAT` (Backwards compatibility guarantees)
3. **HALT RULE**: If any failure mode, partial-write rollback (e.g., S3 succeeds but DB fails), or concurrency race is under-specified in the user's prompt, stop immediately after presenting the System Law Specification and ask the user to resolve the ambiguity. Do NOT write implementation code until the contract is confirmed.

### Phase 2: Executable Oracle First
Once the contract is approved, encode the laws into executable code before writing business logic:
- **Haskell**: Define `newtype` smart constructors. Write `QuickCheck` or `Hedgehog` properties. For DB/S3 interactions, follow the pattern in `examples/haskell-db-s3-statemachine.hs`. Leave the implementation body as `undefined`.
- **Python**: Define strict `Pydantic` models or `icontract` decorators. Write `Hypothesis` `@given` properties or `RuleBasedStateMachine` tests. Leave the implementation body as `raise NotImplementedError`.
- **Infra / CI**: Define policy checks or schema diff assertions.

### Phase 3: Zero-Bloat Implementation
Implement the minimum pure core and thin imperative shell required to satisfy the Phase 2 Oracle:
- Keep business laws in pure functions (`a -> Either DomainError b`).
- Restrict side effects (Postgres queries, S3 uploads, HTTP calls) to a thin boundary layer.
- Keep net Lines of Code (LOC) for bug fixes flat or negative whenever possible.

### Phase 4: Black-Box Verification & CI Loop
**IMPORTANT**: Use bundled scripts as black boxes. Do NOT read the source code of `scripts/verify_gate.sh` unless it fails internally. Always run `--help` first to inspect flags:

```bash
./.agents/skills/formal-spec-verifier/scripts/verify_gate.sh --help
```

1. Run local verification for your target stack (`--haskell`, `--python`, or `--infra`).
2. When pushing changes or verifying a PR, run `./.agents/skills/formal-spec-verifier/scripts/verify_gate.sh --ci` to verify that the remote GitHub Actions pipeline passes.
3. If any check fails, return to **Phase 2** to tighten the property test or type definition before editing implementation code.