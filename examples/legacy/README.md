# examples/legacy/

Older example programs that no longer build cleanly against the current compiler. Kept for historical reference and as a starting point if anyone wants to port them forward.

## Files and why they're here

| File | Issue |
|---|---|
| `calculator.kr` | Parser error at line 35: older syntax for something the current parser doesn't accept. |
| `vec_demo.kr` | Calls `print_int`, which is no longer a registered function. The runtime now expects `puts(fmt_int(x))`. |
| `map_demo.kr` | Same pattern as `vec_demo.kr`: drifted stdlib references. |
| `file_read_simple.kr` | Calls `fgets`, which is no longer registered. Use `file_read_string(path)` instead. |
| `safe_pointers.kr` | Stale stdlib references. |
| `string_processing.kr` | Stale stdlib references. |
| `trait_patterns.kr` | Parser error at line 18: older trait syntax that the current parser doesn't accept. |
| `showcase.kr` | Mix of stale stdlib references and older syntax. |
| `kraken_demo.kr`, `kraken_showcase.kr`, `kraken_final_demo.kr` | Same as `showcase.kr` — large feature dumps written against older versions. |
| `async_workflow.kr` | `channel_send` argument types changed; needs updating. |

## Porting one forward

If you want to revive one of these as a working example, the typical changes are:

1. Replace `print_int(x)` with `puts(fmt_int(x))`.
2. Replace `fgets(...)` with `file_read_string(path)` if reading a whole file.
3. Use `puts(str_concat("label: ", value))` for formatted output.
4. Add explicit `-> int` return type and `return 0;` at the end of `main`.
5. Run `cargo run -p kraken -- run examples/<your_file>.kr` and follow the diagnostic.

If a port works, move it back into `examples/` and add it to the table in `../README.md`.
