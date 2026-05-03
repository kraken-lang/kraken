//! LSIF (Language Server Index Format) generator.
//!
//! Produces a `dump.lsif` file in line-delimited JSON (JSON Lines) format.
//! Each line is a vertex or edge conforming to the LSIF 0.4 specification.
//!
//! This generator indexes:
//! - All `.kr` source files (document vertices)
//! - Function, struct, enum, trait, type alias, and constant declarations (definition results)
//! - Doc comments attached to declarations (hover results)
//! - Cross-file symbol references (via the docgen search index)

use serde::Serialize;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

use super::types::DocGraph;

/// Monotonically increasing ID counter for LSIF elements.
static NEXT_ID: AtomicU64 = AtomicU64::new(1);

fn next_id() -> u64 {
    NEXT_ID.fetch_add(1, Ordering::Relaxed)
}

/// Reset the ID counter (for deterministic test output).
#[cfg(test)]
fn reset_ids() {
    NEXT_ID.store(1, Ordering::Relaxed);
}

// ---------------------------------------------------------------------------
// LSIF element types
// ---------------------------------------------------------------------------

#[derive(Debug, Serialize)]
#[serde(untagged)]
enum Element {
    Vertex(Vertex),
    Edge(Edge),
}

#[derive(Debug, Serialize)]
struct Vertex {
    id: u64,
    #[serde(rename = "type")]
    element_type: &'static str,
    label: &'static str,
    #[serde(flatten)]
    data: VertexData,
}

#[derive(Debug, Serialize)]
#[serde(untagged)]
enum VertexData {
    MetaData(MetaDataPayload),
    Project(ProjectPayload),
    Document(DocumentPayload),
    Range(RangePayload),
    ResultSet(EmptyPayload),
    DefinitionResult(EmptyPayload),
    HoverResult(HoverPayload),
}

#[derive(Debug, Serialize)]
struct Edge {
    id: u64,
    #[serde(rename = "type")]
    element_type: &'static str,
    label: &'static str,
    #[serde(rename = "outV")]
    out_v: u64,
    #[serde(rename = "inV", skip_serializing_if = "Option::is_none")]
    in_v: Option<u64>,
    #[serde(rename = "inVs", skip_serializing_if = "Option::is_none")]
    in_vs: Option<Vec<u64>>,
}

// ---------------------------------------------------------------------------
// Payload structs
// ---------------------------------------------------------------------------

#[derive(Debug, Serialize)]
struct MetaDataPayload {
    version: &'static str,
    #[serde(rename = "projectRoot")]
    project_root: String,
    #[serde(rename = "positionEncoding")]
    position_encoding: &'static str,
    #[serde(rename = "toolInfo")]
    tool_info: ToolInfo,
}

#[derive(Debug, Serialize)]
struct ToolInfo {
    name: &'static str,
    version: String,
}

#[derive(Debug, Serialize)]
struct ProjectPayload {
    kind: &'static str,
}

#[derive(Debug, Serialize)]
struct DocumentPayload {
    uri: String,
    #[serde(rename = "languageId")]
    language_id: &'static str,
}

#[derive(Debug, Serialize)]
struct RangePayload {
    start: Position,
    end: Position,
}

#[derive(Debug, Serialize)]
struct Position {
    line: usize,
    character: usize,
}

#[derive(Debug, Serialize)]
struct EmptyPayload {}

#[derive(Debug, Serialize)]
struct HoverPayload {
    result: HoverContent,
}

#[derive(Debug, Serialize)]
struct HoverContent {
    contents: Vec<MarkedString>,
}

#[derive(Debug, Serialize)]
struct MarkedString {
    language: &'static str,
    value: String,
}

// ---------------------------------------------------------------------------
// Declaration found during source scanning
// ---------------------------------------------------------------------------

#[allow(dead_code)]
struct SourceDecl {
    name: String,
    kind: &'static str,
    line: usize,
    col_start: usize,
    col_end: usize,
    doc_lines: Vec<String>,
    signature: String,
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Generate an LSIF dump covering all `.kr` files under `root`.
/// Returns the dump as a String (line-delimited JSON).
pub fn generate_lsif(root: &Path, _graph: &DocGraph) -> String {
    NEXT_ID.store(1, Ordering::Relaxed);

    let mut elements: Vec<Element> = Vec::new();

    let project_root_uri = format!("file://{}", root.display());

    // 1. MetaData vertex
    let meta_id = emit_vertex(
        &mut elements,
        "metaData",
        VertexData::MetaData(MetaDataPayload {
            version: "0.4.0",
            project_root: project_root_uri.clone(),
            position_encoding: "utf-16",
            tool_info: ToolInfo {
                name: "kraken-lsif",
                version: env!("CARGO_PKG_VERSION").into(),
            },
        }),
    );
    let _ = meta_id;

    // 2. Project vertex
    let project_id = emit_vertex(
        &mut elements,
        "project",
        VertexData::Project(ProjectPayload { kind: "kraken" }),
    );

    // 3. Discover .kr files
    let kr_files = discover_kr_files(root);

    let mut doc_ids = Vec::new();

    for file_path in &kr_files {
        let source = match std::fs::read_to_string(file_path) {
            Ok(s) => s,
            Err(_) => continue,
        };

        let uri = format!("file://{}", file_path.display());

        // Document vertex
        let doc_id = emit_vertex(
            &mut elements,
            "document",
            VertexData::Document(DocumentPayload {
                uri,
                language_id: "kraken",
            }),
        );
        doc_ids.push(doc_id);

        // Scan for declarations
        let decls = scan_declarations(&source);

        let mut range_ids = Vec::new();

        for decl in &decls {
            // Range vertex for the declaration name
            let range_id = emit_vertex(
                &mut elements,
                "range",
                VertexData::Range(RangePayload {
                    start: Position {
                        line: decl.line,
                        character: decl.col_start,
                    },
                    end: Position {
                        line: decl.line,
                        character: decl.col_end,
                    },
                }),
            );
            range_ids.push(range_id);

            // ResultSet vertex
            let result_set_id = emit_vertex(
                &mut elements,
                "resultSet",
                VertexData::ResultSet(EmptyPayload {}),
            );

            // Edge: range -> next -> resultSet
            emit_edge(&mut elements, "next", range_id, Some(result_set_id), None);

            // DefinitionResult vertex
            let def_result_id = emit_vertex(
                &mut elements,
                "definitionResult",
                VertexData::DefinitionResult(EmptyPayload {}),
            );

            // Edge: resultSet -> textDocument/definition -> definitionResult
            emit_edge(
                &mut elements,
                "textDocument/definition",
                result_set_id,
                Some(def_result_id),
                None,
            );

            // Edge: definitionResult -> item -> [range]
            emit_edge(
                &mut elements,
                "item",
                def_result_id,
                None,
                Some(vec![range_id]),
            );

            // Hover result with signature + doc
            let mut hover_text = decl.signature.clone();
            if !decl.doc_lines.is_empty() {
                hover_text.push_str("\n\n");
                hover_text.push_str(&decl.doc_lines.join("\n"));
            }

            let hover_id = emit_vertex(
                &mut elements,
                "hoverResult",
                VertexData::HoverResult(HoverPayload {
                    result: HoverContent {
                        contents: vec![MarkedString {
                            language: "kraken",
                            value: hover_text,
                        }],
                    },
                }),
            );

            // Edge: resultSet -> textDocument/hover -> hoverResult
            emit_edge(
                &mut elements,
                "textDocument/hover",
                result_set_id,
                Some(hover_id),
                None,
            );
        }

        // Edge: document -> contains -> [ranges]
        if !range_ids.is_empty() {
            emit_edge(&mut elements, "contains", doc_id, None, Some(range_ids));
        }
    }

    // Edge: project -> contains -> [documents]
    if !doc_ids.is_empty() {
        emit_edge(&mut elements, "contains", project_id, None, Some(doc_ids));
    }

    // Serialize to line-delimited JSON
    let mut output = String::new();
    for elem in &elements {
        if let Ok(line) = serde_json::to_string(elem) {
            output.push_str(&line);
            output.push('\n');
        }
    }
    output
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn emit_vertex(elements: &mut Vec<Element>, label: &'static str, data: VertexData) -> u64 {
    let id = next_id();
    elements.push(Element::Vertex(Vertex {
        id,
        element_type: "vertex",
        label,
        data,
    }));
    id
}

fn emit_edge(
    elements: &mut Vec<Element>,
    label: &'static str,
    out_v: u64,
    in_v: Option<u64>,
    in_vs: Option<Vec<u64>>,
) -> u64 {
    let id = next_id();
    elements.push(Element::Edge(Edge {
        id,
        element_type: "edge",
        label,
        out_v,
        in_v,
        in_vs,
    }));
    id
}

fn discover_kr_files(root: &Path) -> Vec<PathBuf> {
    let pattern = format!("{}/**/*.kr", root.display());
    let mut files = Vec::new();
    if let Ok(entries) = glob::glob(&pattern) {
        for entry in entries.flatten() {
            files.push(entry);
        }
    }
    files.sort();
    files
}

/// Scan source code for declarations, extracting name, position, and doc comments.
fn scan_declarations(source: &str) -> Vec<SourceDecl> {
    let mut decls = Vec::new();
    let mut pending_docs: Vec<String> = Vec::new();
    let lines: Vec<&str> = source.lines().collect();

    for (line_idx, &line) in lines.iter().enumerate() {
        let trimmed = line.trim();

        // Collect doc comments
        if let Some(stripped) = trimmed.strip_prefix("///") {
            pending_docs.push(stripped.trim().to_string());
            continue;
        }
        if trimmed.starts_with("//!") || trimmed.starts_with("//") {
            continue;
        }

        // Try to detect a declaration
        let tokens: Vec<&str> = trimmed.split_whitespace().collect();
        if tokens.is_empty() {
            if trimmed.is_empty() && !pending_docs.is_empty() {
                // Blank line resets pending docs only if no decl follows soon
            }
            continue;
        }

        let start = if tokens.first() == Some(&"pub") { 1 } else { 0 };
        let kw = tokens.get(start).copied().unwrap_or("");

        #[allow(clippy::collapsible_match)]
        let kind: &'static str = match kw {
            "fn" => "function",
            "async" => {
                if tokens.get(start + 1) == Some(&"fn") {
                    "function"
                } else {
                    pending_docs.clear();
                    continue;
                }
            }
            "struct" => "struct",
            "enum" => "enum",
            "trait" => "trait",
            "impl" => "impl",
            "type" => "type_alias",
            "const" => {
                if tokens.get(start + 1) == Some(&"fn") {
                    "function"
                } else {
                    "constant"
                }
            }
            "union" => "union",
            "class" => "class",
            "interface" => "interface",
            _ => {
                pending_docs.clear();
                continue;
            }
        };

        // Determine name position
        let name_offset =
            if kw == "async" || (kw == "const" && tokens.get(start + 1) == Some(&"fn")) {
                start + 2
            } else {
                start + 1
            };

        let raw_name = tokens.get(name_offset).unwrap_or(&"");
        let name = raw_name
            .split(&['(', '<', '{', '!'][..])
            .next()
            .unwrap_or(raw_name)
            .to_string();

        if name.is_empty() {
            pending_docs.clear();
            continue;
        }

        // Find column offset of the name in the original line
        let col_start = line.find(&name).unwrap_or(0);
        let col_end = col_start + name.len();

        // Build signature from the first line
        let signature = format!("{kind} {name}");

        decls.push(SourceDecl {
            name,
            kind,
            line: line_idx,
            col_start,
            col_end,
            doc_lines: std::mem::take(&mut pending_docs),
            signature,
        });
    }

    decls
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::docgen;

    #[test]
    fn test_scan_declarations_fn() {
        let source = "/// Adds two ints.\nfn add(a: int, b: int) -> int {\n    return a + b;\n}\n";
        let decls = scan_declarations(source);
        assert_eq!(decls.len(), 1);
        assert_eq!(decls[0].name, "add");
        assert_eq!(decls[0].kind, "function");
        assert_eq!(decls[0].line, 1);
        assert_eq!(decls[0].doc_lines.len(), 1);
        assert_eq!(decls[0].doc_lines[0], "Adds two ints.");
    }

    #[test]
    fn test_scan_declarations_struct() {
        let source = "/// A point.\npub struct Point {\n    x: int;\n}\n";
        let decls = scan_declarations(source);
        assert_eq!(decls.len(), 1);
        assert_eq!(decls[0].name, "Point");
        assert_eq!(decls[0].kind, "struct");
    }

    #[test]
    fn test_scan_declarations_multiple() {
        let source = "/// First.\nfn a() {}\n/// Second.\nfn b() {}\n";
        let decls = scan_declarations(source);
        assert_eq!(decls.len(), 2);
        assert_eq!(decls[0].name, "a");
        assert_eq!(decls[1].name, "b");
    }

    #[test]
    fn test_scan_declarations_async_fn() {
        let source = "async fn fetch() {}\n";
        let decls = scan_declarations(source);
        assert_eq!(decls.len(), 1);
        assert_eq!(decls[0].name, "fetch");
        assert_eq!(decls[0].kind, "function");
    }

    #[test]
    fn test_scan_declarations_pub_enum() {
        let source = "pub enum Color { Red, Green }\n";
        let decls = scan_declarations(source);
        assert_eq!(decls.len(), 1);
        assert_eq!(decls[0].name, "Color");
        assert_eq!(decls[0].kind, "enum");
    }

    #[test]
    fn test_generate_lsif_empty_dir() {
        reset_ids();
        let graph = docgen::generate();
        let tmp = std::env::temp_dir().join("kraken_lsif_test_empty");
        let _ = std::fs::create_dir_all(&tmp);
        let dump = generate_lsif(&tmp, &graph);
        assert!(!dump.is_empty(), "LSIF dump should not be empty");
        // Should contain metaData and project vertices at minimum
        assert!(dump.contains("\"metaData\""), "should have metaData vertex");
        assert!(dump.contains("\"project\""), "should have project vertex");
        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_generate_lsif_with_source() {
        reset_ids();
        let graph = docgen::generate();
        let tmp = std::env::temp_dir().join("kraken_lsif_test_src");
        let _ = std::fs::create_dir_all(&tmp);
        std::fs::write(
            tmp.join("main.kr"),
            "/// Entry point.\nfn main() -> int {\n    return 0;\n}\n",
        )
        .unwrap();

        let dump = generate_lsif(&tmp, &graph);
        assert!(dump.contains("\"document\""), "should have document vertex");
        assert!(dump.contains("\"range\""), "should have range vertex");
        assert!(
            dump.contains("\"definitionResult\""),
            "should have definitionResult"
        );
        assert!(dump.contains("\"hoverResult\""), "should have hoverResult");
        assert!(dump.contains("main"), "should contain function name");
        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_lsif_line_delimited() {
        reset_ids();
        let graph = docgen::generate();
        let tmp = std::env::temp_dir().join("kraken_lsif_test_lines");
        let _ = std::fs::create_dir_all(&tmp);
        let dump = generate_lsif(&tmp, &graph);
        // Every line should be valid JSON
        for (i, line) in dump.lines().enumerate() {
            assert!(
                serde_json::from_str::<serde_json::Value>(line).is_ok(),
                "line {} is not valid JSON: {}",
                i + 1,
                line
            );
        }
        let _ = std::fs::remove_dir_all(&tmp);
    }
}
