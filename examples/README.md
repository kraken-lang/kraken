# Examples

Small, self-contained Kraken programs that exercise a single feature or pattern. Each one is verified to build and run on the current `main` (`v0.9.3`).

## Running

```bash
# Compile and run a single example
cargo run -p kraken -- run examples/hello.kr

# Or build to ./build/<name>(.exe) and run separately
cargo run -p kraken -- build examples/hello.kr
./build/hello
```

## Available examples

| File | Shows |
|---|---|
| [hello.kr](hello.kr) | The minimum: `fn main() -> int` returning `0`. |
| [hello_world.kr](hello_world.kr) | Same, plus `puts` for stdout. |
| [closures.kr](closures.kr) | Closure literals, `fn(int) -> int` types, higher-order functions. |
| [enum_match.kr](enum_match.kr) | Enum declarations and `match` arms with enum patterns. |
| [vec_basics.kr](vec_basics.kr) | `VecInt` and `VecString` from the runtime: push, get, len, free. |
| [strings_basic.kr](strings_basic.kr) | `str_concat`, `strlen`, `str_slice`, `str_starts_with`, `str_contains`, `str_eq`. |
| [assert_example.kr](assert_example.kr) | `assert`, `assert_eq`, `assert_ne` from the test framework. |
| [modules_example_main.kr](modules_example_main.kr) + [modules_example_util.kr](modules_example_util.kr) | Two-file module example with `import`. Returns 42 on success. |

## legacy/

[`legacy/`](legacy/) holds older example programs from earlier versions of Kraken that no longer build cleanly against the current compiler. They were written against APIs that have since been renamed (`print_int`, `fgets`, channel signatures, etc.) or use older syntax. Kept for historical reference; not built as part of the test suite. See [`legacy/README.md`](legacy/README.md) for the per-file status.

## Adding a new example

A good example is short, has one clear purpose, and runs cleanly. Conventions used here:

- Keep it under ~50 lines
- Always declare `fn main() -> int { ...; return 0; }` (explicit return type and return value)
- Top-of-file comment explains what it shows and how to run it
- Prefer functions that exist in the runtime (`puts`, `str_concat`, `fmt_int`, etc.) over assuming a stdlib import
- Verify it builds clean before committing: `cargo run -p kraken -- run examples/<name>.kr`
