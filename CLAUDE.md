# CLAUDE.md — Kraken (Bootstrap Compiler)

> Operating manual for any AI assistant working in this repository.
> Read this file in full before doing anything else.

## Required reading order

Before touching any code or generating any output, read in order:

1. **`.dev/REPS.md`** — the engineering standards. Authoritative. Non-negotiable.
2. **`.dev/DIRECTIVES.md`** — directives specific to this repo and to AI-assisted work.
3. **`.dev/AUDIT.md`** — current verifiable state of the repo. Read this to ground yourself in what exists vs. what is planned.
4. **`.dev/ROADMAP.md`** — what is next, what is out of scope.
5. **This file** — operating instructions, build commands, communication style.

If any of those files are missing, stop and tell the user. Do not improvise.

## What this repo is

This is the **bootstrap Kraken compiler** — written in Rust, targeting LLVM 18 — used to compile Kraken source code (`.kr`) into native binaries. It is the parent compiler that bootstrapped the self-hosted compiler in the sibling `krakenc` repo.

- Language: Rust (workspace)
- Backend: LLVM 18 via `llvm-sys`
- Status: production-quality bootstrap, actively maintained
- Sibling repo: `kraken-lang/krakenc` (the self-hosted compiler written in Kraken itself)

## Workspace structure

```
compiler/    — Rust implementation of the compiler (parser, type checker, codegen)
runtime/     — C runtime library and FFI bindings
stdlib/      — Kraken stdlib written in Kraken (option, result, traits)
examples/    — Example Kraken programs
tests/       — Integration tests, fixtures, IR snapshots, stress tests
fuzz/        — Fuzzing targets (cargo-fuzz)
docs/        — Public documentation (architecture, spec, traits, containers)
.dev/        — Internal AI-facing documentation (this folder)
```

## Build commands

```bash
# Build the bootstrap compiler
cargo build -p kraken

# Strict build (REPS-mandated)
cargo fmt --check
RUSTFLAGS="-D warnings" cargo test --workspace
RUSTFLAGS="-D warnings" cargo clippy --workspace --all-targets --all-features

# Run an example
cargo run -p kraken -- build examples/hello.kr
./build/hello

# Benchmark
cargo bench
```

## What you may do without asking

- Read any file in the repo
- Run `cargo build`, `cargo test`, `cargo clippy`, `cargo fmt --check`
- Propose patches as inline diffs or suggested edits
- Cross-reference REPS sections by name when explaining a recommendation
- Update `.dev/AUDIT.md` when verifiable state of the repo has changed

## What you must not do without explicit approval

- Modify `.dev/REPS.md` — it is authoritative; only the user updates it
- Add dependencies to `Cargo.toml` (REPS — Dependency Management)
- Introduce `unsafe` code without a `// SAFETY:` block (REPS — Unsafe Code)
- Use `unwrap()` or `expect()` in non-test code (REPS — Code Quality)
- Commit or push — the user runs all git operations
- Modify CI configuration
- Touch `LICENSE`, `CHANGELOG.md` (user maintains the changelog), or release-related files
- Generate "AI-style" documentation (no "comprehensive", "robust", "seamless", "leverage", emoji, or "Phase X / Step Y" headers — see REPS — Documentation)

## Communication style

- Be concrete. Cite file paths, line numbers, and REPS section names.
- When proposing changes, explain the *why* by reference to a REPS clause.
- Match the user's energy — brief when he is brief, detailed when the question requires it.
- Do not generate filler. Do not pad responses with caveats unless the caveat is material.
- If something is uncertain, say so explicitly. Do not invent.

## When the user asks for a change

1. Read the relevant file(s) before proposing changes. Do not write blind patches.
2. Identify which REPS sections govern the change.
3. Propose the smallest correct change. Avoid scope creep.
4. State explicitly what tests need to pass for the change to be acceptable.
5. If the change touches a hot path or public API, call that out.

## Relationship to `krakenc`

This repo (`kraken`) compiles Kraken source via Rust+LLVM. The sibling repo (`krakenc`) is the self-hosted compiler written in Kraken itself, which currently emits C and is bootstrapped through this compiler.

The long-term plan is for `krakenc` to gain its own LLVM backend and become independent. At that point, this repo's role narrows to a maintained reference implementation. See `.dev/ROADMAP.md`.

## When in doubt

Ask the user. The cost of one clarifying question is far lower than the cost of a confidently wrong implementation.
