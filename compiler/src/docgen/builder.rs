//! DocGraph builder — populates all nodes, search entries, and pages.

use super::types::*;
use std::collections::BTreeMap;

/// Build the complete DocGraph for the Kraken language.
pub fn generate() -> DocGraph {
    let mut nodes = BTreeMap::new();
    let mut search = Vec::new();

    add_keywords(&mut nodes, &mut search);
    add_operators(&mut nodes, &mut search);
    add_primitive_types(&mut nodes, &mut search);
    add_container_types(&mut nodes, &mut search);
    add_stdlib_functions(&mut nodes, &mut search);
    add_cli_tooling(&mut nodes, &mut search);

    let node_kinds = {
        let mut k: Vec<String> = nodes.values().map(|n| n.kind.clone()).collect();
        k.sort();
        k.dedup();
        k
    };
    let tags = {
        let mut t: Vec<String> = nodes
            .values()
            .filter_map(|n| n.tags.as_ref())
            .flatten()
            .cloned()
            .collect();
        t.sort();
        t.dedup();
        t
    };

    let pages = build_pages(&nodes);

    let now = {
        use std::time::{SystemTime, UNIX_EPOCH};
        let secs = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs();
        format!("{secs}")
    };

    DocGraph {
        meta: Meta {
            schema: SchemaRef {
                name: "kraken-docgraph",
                version: 1,
            },
            generated_at: now,
            tool: ToolRef {
                name: "kraken-docgen".into(),
                version: env!("CARGO_PKG_VERSION").into(),
            },
            source: SourceRef {
                project: "kraken-lang/kraken".into(),
                revision: "HEAD".into(),
            },
        },
        index: Index {
            node_kinds,
            tags,
            search: SearchIndex { entries: search },
        },
        pages,
        nodes,
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn mk_node(id: &str, kind: &str, title: &str, since: &str, tags: &[&str]) -> Node {
    Node {
        id: id.into(),
        kind: kind.into(),
        title: title.into(),
        qualified_name: None,
        stability: "stable".into(),
        since: Some(since.into()),
        tags: Some(tags.iter().map(|s| s.to_string()).collect()),
        docs: None,
        symbol: None,
        type_node: None,
        diagnostic: None,
        concept: None,
        tooling: None,
        links: None,
    }
}

fn mk_search(id: &str, title: &str, kind: &str, path: &str, tokens: &[&str]) -> SearchEntry {
    SearchEntry {
        node_id: id.into(),
        title: title.into(),
        kind: kind.into(),
        path: path.into(),
        tokens: tokens.iter().map(|s| s.to_string()).collect(),
    }
}

// ---------------------------------------------------------------------------
// Keywords
// ---------------------------------------------------------------------------

fn add_keywords(nodes: &mut BTreeMap<String, Node>, search: &mut Vec<SearchEntry>) {
    let kws: Vec<(&str, &str, &str)> = vec![
        ("if", "Conditional branch", "control-flow"),
        ("else", "Alternative branch", "control-flow"),
        ("match", "Pattern matching", "control-flow"),
        ("for", "C-style loop", "control-flow"),
        ("while", "Condition loop", "control-flow"),
        ("do", "Do loop (reserved)", "control-flow"),
        ("break", "Exit loop", "control-flow"),
        ("continue", "Skip iteration", "control-flow"),
        ("return", "Return from function", "control-flow"),
        ("fn", "Function declaration", "declaration"),
        ("let", "Variable declaration", "declaration"),
        ("const", "Constant declaration", "declaration"),
        ("struct", "Struct type declaration", "declaration"),
        ("enum", "Enum type declaration", "declaration"),
        ("trait", "Trait declaration", "declaration"),
        ("impl", "Implementation block", "declaration"),
        ("type", "Type alias", "declaration"),
        ("union", "Union type declaration", "declaration"),
        ("class", "Class declaration", "declaration"),
        ("interface", "Interface declaration", "declaration"),
        ("module", "Module declaration", "module"),
        ("import", "Import declaration", "module"),
        ("pub", "Public visibility", "modifier"),
        ("priv", "Private visibility", "modifier"),
        ("static", "Static qualifier", "modifier"),
        ("async", "Async function", "modifier"),
        ("await", "Await expression", "modifier"),
        ("spawn", "Spawn concurrent task", "modifier"),
        ("ref", "Reference qualifier", "modifier"),
        ("mut", "Mutable qualifier", "modifier"),
        ("move", "Move capture", "modifier"),
        ("unsafe", "Unsafe block/function", "modifier"),
        ("true", "Boolean literal true", "literal"),
        ("false", "Boolean literal false", "literal"),
        ("null", "Null pointer value", "literal"),
        ("self", "Self reference", "special"),
        ("super", "Parent module", "special"),
        ("defer", "Deferred execution", "special"),
        ("generic", "Generic qualifier", "generics"),
        ("where", "Generic constraint", "generics"),
        ("as", "Type cast", "special"),
        ("in", "Iterator variable", "special"),
        ("dyn", "Trait object", "generics"),
        ("macro", "Macro keyword", "macro"),
        ("macro_rules", "Declarative macro", "macro"),
        ("derive", "Derive attribute", "macro"),
        ("inline", "Inline hint", "macro"),
        ("no_mangle", "No name mangling", "macro"),
        ("test", "Test function", "macro"),
        ("static_assert", "Compile-time assertion", "macro"),
    ];
    for (kw, desc, group) in kws {
        let id = format!("keyword.{kw}");
        let mut n = mk_node(&id, "keyword", &format!("`{kw}`"), "0.1.0", &["keyword", group]);
        n.docs = Some(DocBlock {
            summary: Some(desc.into()),
            details_markdown: None,
            examples: None,
            see_also: None,
        });
        n.concept = Some(ConceptNode {
            group: Some(group.into()),
            keywords: Some(vec![kw.into()]),
        });
        search.push(mk_search(
            &id,
            &format!("`{kw}` keyword"),
            "keyword",
            &format!("keywords/{kw}"),
            &[kw, "keyword", group],
        ));
        nodes.insert(id, n);
    }
}

// ---------------------------------------------------------------------------
// Operators
// ---------------------------------------------------------------------------

fn add_operators(nodes: &mut BTreeMap<String, Node>, search: &mut Vec<SearchEntry>) {
    let ops: Vec<(&str, &str, &str)> = vec![
        ("plus", "+", "Addition / unary plus"),
        ("minus", "-", "Subtraction / unary negation"),
        ("star", "*", "Multiplication / dereference"),
        ("slash", "/", "Division"),
        ("percent", "%", "Remainder"),
        ("equal", "==", "Equality comparison"),
        ("not-equal", "!=", "Inequality comparison"),
        ("less", "<", "Less than"),
        ("less-equal", "<=", "Less than or equal"),
        ("greater", ">", "Greater than"),
        ("greater-equal", ">=", "Greater than or equal"),
        ("and", "&&", "Logical AND (short-circuit)"),
        ("or", "||", "Logical OR (short-circuit)"),
        ("not", "!", "Logical NOT"),
        ("bit-and", "&", "Bitwise AND / reference"),
        ("bit-or", "|", "Bitwise OR"),
        ("bit-xor", "^", "Bitwise XOR"),
        ("bit-not", "~", "Bitwise NOT"),
        ("left-shift", "<<", "Left bit shift"),
        ("right-shift", ">>", "Arithmetic right shift"),
        ("assign", "=", "Assignment"),
        ("plus-assign", "+=", "Addition assignment"),
        ("minus-assign", "-=", "Subtraction assignment"),
        ("star-assign", "*=", "Multiplication assignment"),
        ("slash-assign", "/=", "Division assignment"),
        ("percent-assign", "%=", "Remainder assignment"),
        ("range", "..", "Exclusive range"),
        ("range-inclusive", "..=", "Inclusive range"),
        ("try", "?", "Try operator (error propagation)"),
        ("arrow", "->", "Return type / match arm"),
        ("double-colon", "::", "Path separator / turbofish"),
    ];
    for (slug, sym, desc) in ops {
        let id = format!("operator.{slug}");
        let mut n = mk_node(&id, "operator", &format!("`{sym}`"), "0.1.0", &["operator"]);
        n.docs = Some(DocBlock {
            summary: Some(desc.into()),
            details_markdown: None,
            examples: None,
            see_also: None,
        });
        search.push(mk_search(
            &id,
            &format!("`{sym}` operator"),
            "operator",
            &format!("operators/{slug}"),
            &[sym, slug, "operator"],
        ));
        nodes.insert(id, n);
    }
}

// ---------------------------------------------------------------------------
// Primitive types
// ---------------------------------------------------------------------------

fn add_primitive_types(nodes: &mut BTreeMap<String, Node>, search: &mut Vec<SearchEntry>) {
    let prims: Vec<(&str, &str)> = vec![
        ("int", "64-bit signed integer (i64)"),
        ("float", "64-bit IEEE 754 double-precision"),
        ("bool", "Boolean value (true/false)"),
        ("string", "Null-terminated C string pointer (i8*)"),
        ("str", "Borrowed string view (read-only i8*)"),
        ("bytes", "Raw byte buffer pointer (i8*)"),
        ("void", "No value; used for functions returning nothing"),
    ];
    for (name, desc) in prims {
        let id = format!("type.{name}");
        let mut n = mk_node(&id, "type", name, "0.1.0", &["type", "primitive"]);
        n.type_node = Some(TypeNode {
            kind: "primitive".into(),
            fields: None,
            variants: None,
        });
        n.docs = Some(DocBlock {
            summary: Some(desc.into()),
            details_markdown: None,
            examples: None,
            see_also: None,
        });
        search.push(mk_search(
            &id,
            name,
            "type",
            &format!("types/{name}"),
            &[name, "primitive", "type"],
        ));
        nodes.insert(id, n);
    }
}

// ---------------------------------------------------------------------------
// Container types
// ---------------------------------------------------------------------------

fn add_container_types(nodes: &mut BTreeMap<String, Node>, search: &mut Vec<SearchEntry>) {
    let containers: Vec<(&str, &str)> = vec![
        ("VecInt", "Dynamic array of integers"),
        ("VecString", "Dynamic array of strings"),
        ("VecBytes", "Dynamic array of byte buffers"),
        ("MapStringInt", "Hash map: string keys, int values"),
        ("MapStringString", "Hash map: string keys, string values"),
        ("SliceInt", "Borrowed view of integers"),
        ("SliceString", "Borrowed view of strings"),
        ("SliceBytes", "Borrowed view of byte buffers"),
    ];
    for (name, desc) in containers {
        let id = format!("type.{}", name.to_lowercase());
        let mut n = mk_node(&id, "type", name, "0.8.0", &["type", "container"]);
        n.type_node = Some(TypeNode {
            kind: "struct".into(),
            fields: None,
            variants: None,
        });
        n.docs = Some(DocBlock {
            summary: Some(desc.into()),
            details_markdown: None,
            examples: None,
            see_also: None,
        });
        search.push(mk_search(
            &id,
            name,
            "type",
            &format!("types/{name}"),
            &[name, "container", "type"],
        ));
        nodes.insert(id, n);
    }
}

// ---------------------------------------------------------------------------
// Stdlib functions
// ---------------------------------------------------------------------------

fn add_stdlib_functions(nodes: &mut BTreeMap<String, Node>, search: &mut Vec<SearchEntry>) {
    struct FnDef {
        name: &'static str,
        sig: &'static str,
        desc: &'static str,
        params: Vec<(&'static str, &'static str)>,
        ret: &'static str,
    }
    let fns = vec![
        FnDef { name: "printf", sig: "fn printf(fmt: string, ...) -> int", desc: "Print formatted output to stdout", params: vec![("fmt", "string")], ret: "int" },
        FnDef { name: "puts", sig: "fn puts(s: string) -> int", desc: "Print string with newline", params: vec![("s", "string")], ret: "int" },
        FnDef { name: "malloc", sig: "fn malloc(size: int) -> bytes", desc: "Allocate heap memory", params: vec![("size", "int")], ret: "bytes" },
        FnDef { name: "calloc", sig: "fn calloc(count: int, size: int) -> bytes", desc: "Allocate zeroed heap memory", params: vec![("count", "int"), ("size", "int")], ret: "bytes" },
        FnDef { name: "realloc", sig: "fn realloc(ptr: bytes, size: int) -> bytes", desc: "Resize heap allocation", params: vec![("ptr", "bytes"), ("size", "int")], ret: "bytes" },
        FnDef { name: "free", sig: "fn free(ptr: bytes) -> void", desc: "Free heap memory", params: vec![("ptr", "bytes")], ret: "void" },
        FnDef { name: "strlen", sig: "fn strlen(s: string) -> int", desc: "String byte length", params: vec![("s", "string")], ret: "int" },
        FnDef { name: "strcmp", sig: "fn strcmp(a: string, b: string) -> int", desc: "Compare two strings", params: vec![("a", "string"), ("b", "string")], ret: "int" },
        FnDef { name: "kraken_str_split", sig: "fn kraken_str_split(s: string, delim: string) -> VecString", desc: "Split string by delimiter", params: vec![("s", "string"), ("delim", "string")], ret: "VecString" },
        FnDef { name: "kraken_str_join", sig: "fn kraken_str_join(v: VecString, sep: string) -> string", desc: "Join string vector with separator", params: vec![("v", "VecString"), ("sep", "string")], ret: "string" },
        FnDef { name: "kraken_str_len", sig: "fn kraken_str_len(s: string) -> int", desc: "Safe string length", params: vec![("s", "string")], ret: "int" },
        FnDef { name: "kraken_str_concat", sig: "fn kraken_str_concat(a: string, b: string) -> string", desc: "Concatenate two strings (heap-allocated)", params: vec![("a", "string"), ("b", "string")], ret: "string" },
        FnDef { name: "kraken_str_contains", sig: "fn kraken_str_contains(s: string, needle: string) -> bool", desc: "Check if string contains substring", params: vec![("s", "string"), ("needle", "string")], ret: "bool" },
        FnDef { name: "kraken_str_trim", sig: "fn kraken_str_trim(s: string) -> string", desc: "Trim leading and trailing whitespace", params: vec![("s", "string")], ret: "string" },
    ];
    for f in fns {
        let id = format!("fn.{}", f.name);
        let mut n = mk_node(&id, "symbol", f.name, "0.8.0", &["function", "stdlib"]);
        n.symbol = Some(SymbolNode {
            kind: "fn".into(),
            signature: Some(Signature {
                text: Some(f.sig.into()),
                params: Some(
                    f.params
                        .iter()
                        .map(|(pn, pt)| Param {
                            name: pn.to_string(),
                            param_type: TypeRef {
                                display: pt.to_string(),
                            },
                        })
                        .collect(),
                ),
                returns: Some(TypeRef {
                    display: f.ret.into(),
                }),
            }),
            visibility: Some("public".into()),
        });
        n.docs = Some(DocBlock {
            summary: Some(f.desc.into()),
            details_markdown: None,
            examples: None,
            see_also: None,
        });
        search.push(mk_search(
            &id,
            f.name,
            "symbol",
            &format!("std/{}", f.name),
            &[f.name, "function", "stdlib"],
        ));
        nodes.insert(id, n);
    }
}

// ---------------------------------------------------------------------------
// CLI tooling
// ---------------------------------------------------------------------------

fn add_cli_tooling(nodes: &mut BTreeMap<String, Node>, search: &mut Vec<SearchEntry>) {
    let cmds = vec![
        ("build", "Compile a Kraken project"),
        ("run", "Build and execute"),
        ("test", "Run test suite"),
        ("bench", "Run benchmarks with statistical analysis"),
        ("check", "Type-check without emitting binaries"),
        ("fmt", "Format source files"),
        ("doc", "Generate HTML documentation"),
        ("clean", "Remove build artifacts"),
        ("init", "Initialize project in current directory"),
        ("new", "Create new project from template"),
        ("version", "Show compiler version"),
    ];

    let id = "tooling.cli".to_string();
    let mut n = mk_node(&id, "tooling", "Kraken CLI", "0.8.49", &["tooling", "cli"]);
    n.tooling = Some(ToolingNode {
        commands: Some(
            cmds.iter()
                .map(|(name, summary)| ToolCommand {
                    name: name.to_string(),
                    summary: Some(summary.to_string()),
                })
                .collect(),
        ),
    });
    n.docs = Some(DocBlock {
        summary: Some("The `krakenc` command-line compiler and toolchain.".into()),
        details_markdown: Some("Usage: `krakenc <COMMAND> [OPTIONS]`".into()),
        examples: Some(vec![
            Example {
                title: "Build a project".into(),
                code: "krakenc build".into(),
                language: Some("bash".into()),
            },
            Example {
                title: "Run tests".into(),
                code: "krakenc test".into(),
                language: Some("bash".into()),
            },
        ]),
        see_also: None,
    });
    search.push(mk_search(
        &id,
        "Kraken CLI",
        "tooling",
        "tooling/cli",
        &["cli", "krakenc", "tooling", "compiler"],
    ));
    nodes.insert(id, n);
}

// ---------------------------------------------------------------------------
// Pages
// ---------------------------------------------------------------------------

fn build_pages(nodes: &BTreeMap<String, Node>) -> Vec<Page> {
    let mut pages = Vec::new();

    // Keywords page
    let kw_ids: Vec<String> = nodes
        .keys()
        .filter(|k| k.starts_with("keyword."))
        .cloned()
        .collect();
    pages.push(Page {
        id: "page.keywords".into(),
        slug: "keywords".into(),
        title: "Keywords".into(),
        summary: Some("All reserved keywords in the Kraken language.".into()),
        nav: Some(Nav {
            group: Some("Language Reference".into()),
            order: Some(1),
            parent_page_id: None,
        }),
        sections: vec![PageSection {
            kind: "node_list".into(),
            title: Some("Keywords".into()),
            markdown: None,
            node_ids: Some(kw_ids),
        }],
    });

    // Operators page
    let op_ids: Vec<String> = nodes
        .keys()
        .filter(|k| k.starts_with("operator."))
        .cloned()
        .collect();
    pages.push(Page {
        id: "page.operators".into(),
        slug: "operators".into(),
        title: "Operators".into(),
        summary: Some("All operators in the Kraken language.".into()),
        nav: Some(Nav {
            group: Some("Language Reference".into()),
            order: Some(2),
            parent_page_id: None,
        }),
        sections: vec![PageSection {
            kind: "node_list".into(),
            title: Some("Operators".into()),
            markdown: None,
            node_ids: Some(op_ids),
        }],
    });

    // Types page
    let type_ids: Vec<String> = nodes
        .keys()
        .filter(|k| k.starts_with("type."))
        .cloned()
        .collect();
    pages.push(Page {
        id: "page.types".into(),
        slug: "types".into(),
        title: "Types".into(),
        summary: Some("Primitive and container types in Kraken.".into()),
        nav: Some(Nav {
            group: Some("Language Reference".into()),
            order: Some(3),
            parent_page_id: None,
        }),
        sections: vec![PageSection {
            kind: "node_list".into(),
            title: Some("Types".into()),
            markdown: None,
            node_ids: Some(type_ids),
        }],
    });

    // Stdlib page
    let fn_ids: Vec<String> = nodes
        .keys()
        .filter(|k| k.starts_with("fn."))
        .cloned()
        .collect();
    pages.push(Page {
        id: "page.stdlib".into(),
        slug: "stdlib".into(),
        title: "Standard Library".into(),
        summary: Some("Built-in and runtime library functions.".into()),
        nav: Some(Nav {
            group: Some("API Reference".into()),
            order: Some(4),
            parent_page_id: None,
        }),
        sections: vec![PageSection {
            kind: "node_list".into(),
            title: Some("Functions".into()),
            markdown: None,
            node_ids: Some(fn_ids),
        }],
    });

    // Tooling page
    let tool_ids: Vec<String> = nodes
        .keys()
        .filter(|k| k.starts_with("tooling."))
        .cloned()
        .collect();
    pages.push(Page {
        id: "page.tooling".into(),
        slug: "tooling".into(),
        title: "Tooling".into(),
        summary: Some("CLI commands and developer tools.".into()),
        nav: Some(Nav {
            group: Some("Tooling".into()),
            order: Some(5),
            parent_page_id: None,
        }),
        sections: vec![PageSection {
            kind: "node_list".into(),
            title: Some("Tools".into()),
            markdown: None,
            node_ids: Some(tool_ids),
        }],
    });

    pages
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_produces_non_empty_graph() {
        let graph = generate();
        assert!(!graph.nodes.is_empty(), "nodes should not be empty");
        assert!(!graph.pages.is_empty(), "pages should not be empty");
        assert!(
            !graph.index.search.entries.is_empty(),
            "search index should not be empty"
        );
        assert!(
            !graph.index.node_kinds.is_empty(),
            "node_kinds should not be empty"
        );
    }

    #[test]
    fn test_all_keywords_present() {
        let graph = generate();
        let expected = vec![
            "if", "else", "match", "for", "while", "fn", "let", "const", "struct", "enum",
            "trait", "impl", "type", "pub", "async", "await", "unsafe", "mut", "return",
        ];
        for kw in expected {
            let id = format!("keyword.{kw}");
            assert!(graph.nodes.contains_key(&id), "missing keyword node: {kw}");
        }
    }

    #[test]
    fn test_all_primitive_types_present() {
        let graph = generate();
        for ty in &["int", "float", "bool", "string", "str", "bytes", "void"] {
            let id = format!("type.{ty}");
            assert!(
                graph.nodes.contains_key(&id),
                "missing primitive type: {ty}"
            );
        }
    }

    #[test]
    fn test_container_types_present() {
        let graph = generate();
        for name in &[
            "vecint",
            "vecstring",
            "vecbytes",
            "mapstringint",
            "mapstringstring",
        ] {
            let id = format!("type.{name}");
            assert!(
                graph.nodes.contains_key(&id),
                "missing container type: {name}"
            );
        }
    }

    #[test]
    fn test_stdlib_functions_present() {
        let graph = generate();
        for f in &["printf", "malloc", "free", "kraken_str_split", "kraken_str_join"] {
            let id = format!("fn.{f}");
            assert!(
                graph.nodes.contains_key(&id),
                "missing stdlib function: {f}"
            );
        }
    }

    #[test]
    fn test_cli_tooling_present() {
        let graph = generate();
        assert!(
            graph.nodes.contains_key("tooling.cli"),
            "missing CLI tooling node"
        );
        let cli = &graph.nodes["tooling.cli"];
        let cmds = cli
            .tooling
            .as_ref()
            .unwrap()
            .commands
            .as_ref()
            .unwrap();
        assert!(cmds.len() >= 10, "should have at least 10 CLI commands");
    }

    #[test]
    fn test_search_entries_match_nodes() {
        let graph = generate();
        for entry in &graph.index.search.entries {
            assert!(
                graph.nodes.contains_key(&entry.node_id),
                "search entry references missing node: {}",
                entry.node_id
            );
        }
    }

    #[test]
    fn test_pages_reference_existing_nodes() {
        let graph = generate();
        for page in &graph.pages {
            for section in &page.sections {
                if let Some(ids) = &section.node_ids {
                    for id in ids {
                        assert!(
                            graph.nodes.contains_key(id),
                            "page {} section references missing node: {}",
                            page.id,
                            id
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn test_serialization_roundtrip() {
        let graph = generate();
        let json = serde_json::to_string_pretty(&graph).expect("serialization failed");
        assert!(json.len() > 1000, "serialized JSON should be substantial");
        assert!(json.contains("\"keyword\""), "JSON should contain keyword kind");
        assert!(json.contains("\"operator\""), "JSON should contain operator kind");
        assert!(json.contains("\"type\""), "JSON should contain type kind");
    }
}
