//! Documentation metadata generation for the Kraken Language runtime.
//!
//! This module provides functionality to export documentation metadata in the
//! DocGraph v1 format, enabling automatic documentation generation and LSP integration.

#![allow(dead_code)]

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Stability level for documented items.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Stability {
    Stable,
    Experimental,
    Deprecated,
    Internal,
}

/// Node kind in the documentation graph.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum NodeKind {
    Module,
    Symbol,
    Type,
    Trait,
    Const,
    Macro,
    Keyword,
    Operator,
    Attribute,
    Diagnostic,
    Concept,
    Tooling,
}

/// Source location span.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Span {
    pub file: String,
    pub start: Position,
    pub end: Position,
}

/// Position in source code.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Position {
    pub line: usize,
    pub col: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub byte: Option<usize>,
}

/// Deprecation information.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Deprecation {
    pub since: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub note: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub replacement_node_id: Option<String>,
}

/// Documentation block.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DocBlock {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub summary: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub details_markdown: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub examples: Option<Vec<Example>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub see_also: Option<Vec<String>>,
}

/// Code example.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Example {
    pub title: String,
    pub code: String,
    #[serde(default = "default_language")]
    pub language: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub expected_output: Option<String>,
    #[serde(default)]
    pub runnable: bool,
}

fn default_language() -> String {
    "kraken".to_string()
}

/// Type reference.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeRef {
    pub display: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub node_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub args: Option<Vec<TypeRef>>,
}

/// Function parameter.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Param {
    pub name: String,
    #[serde(rename = "type")]
    pub type_ref: TypeRef,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub default: Option<String>,
    #[serde(default)]
    pub variadic: bool,
}

/// Generic parameter.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenericParam {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub constraints: Option<Vec<TypeRef>>,
}

/// Function signature.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Signature {
    pub text: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub params: Option<Vec<Param>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub returns: Option<TypeRef>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub generics: Option<Vec<GenericParam>>,
}

/// Symbol node (function, method, variable, field, enum variant).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SymbolNode {
    pub kind: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub module_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub signature: Option<Signature>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub visibility: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub type_id: Option<String>,
}

/// Module node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleNode {
    pub path: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parent_module_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub children_module_ids: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub exports: Option<Vec<String>>,
}

/// Documentation node.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Node {
    pub id: String,
    pub kind: NodeKind,
    pub title: String,
    pub stability: Stability,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub qualified_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub since: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub deprecated: Option<Deprecation>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tags: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source_span: Option<Span>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub docs: Option<DocBlock>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub module: Option<ModuleNode>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub symbol: Option<SymbolNode>,
}

/// Search index entry.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchEntry {
    pub node_id: String,
    pub title: String,
    pub kind: String,
    pub path: String,
    pub tokens: Vec<String>,
    #[serde(default = "default_boost")]
    pub boost: f64,
}

fn default_boost() -> f64 {
    1.0
}

/// Search index.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchIndex {
    pub entries: Vec<SearchEntry>,
}

/// Index metadata.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Index {
    pub node_kinds: Vec<String>,
    pub tags: Vec<String>,
    pub search: SearchIndex,
}

/// Schema metadata.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SchemaInfo {
    pub name: String,
    pub version: u32,
}

/// Tool metadata.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolInfo {
    pub name: String,
    pub version: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub commit: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub target_triple: Option<String>,
}

/// Source metadata.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SourceInfo {
    pub project: String,
    pub revision: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dirty: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub workspace_root: Option<String>,
}

/// Metadata header.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Meta {
    pub schema: SchemaInfo,
    pub generated_at: String,
    pub tool: ToolInfo,
    pub source: SourceInfo,
    #[serde(default = "default_deterministic")]
    pub deterministic: bool,
}

fn default_deterministic() -> bool {
    true
}

/// Page section.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PageSection {
    pub kind: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub markdown: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub node_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub node_ids: Option<Vec<String>>,
}

/// Documentation page.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Page {
    pub id: String,
    pub slug: String,
    pub title: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub summary: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tags: Option<Vec<String>>,
    pub sections: Vec<PageSection>,
}

/// Complete DocGraph document.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DocGraph {
    pub meta: Meta,
    pub index: Index,
    pub pages: Vec<Page>,
    pub nodes: HashMap<String, Node>,
}

impl DocGraph {
    /// Creates a new DocGraph with the given metadata.
    pub fn new(project: String, version: String) -> Self {
        let now = chrono::Utc::now().to_rfc3339();

        DocGraph {
            meta: Meta {
                schema: SchemaInfo {
                    name: "kraken-docgraph".to_string(),
                    version: 1,
                },
                generated_at: now,
                tool: ToolInfo {
                    name: "kraken-docgen".to_string(),
                    version: version.clone(),
                    commit: None,
                    target_triple: None,
                },
                source: SourceInfo {
                    project,
                    revision: version,
                    dirty: None,
                    workspace_root: None,
                },
                deterministic: true,
            },
            index: Index {
                node_kinds: Vec::new(),
                tags: Vec::new(),
                search: SearchIndex {
                    entries: Vec::new(),
                },
            },
            pages: Vec::new(),
            nodes: HashMap::new(),
        }
    }

    /// Adds a node to the graph.
    pub fn add_node(&mut self, node: Node) {
        let kind_str = format!("{:?}", node.kind).to_lowercase();
        if !self.index.node_kinds.contains(&kind_str) {
            self.index.node_kinds.push(kind_str.clone());
        }

        if let Some(tags) = &node.tags {
            for tag in tags {
                if !self.index.tags.contains(tag) {
                    self.index.tags.push(tag.clone());
                }
            }
        }

        self.nodes.insert(node.id.clone(), node);
    }

    /// Adds a page to the graph.
    pub fn add_page(&mut self, page: Page) {
        self.pages.push(page);
    }

    /// Adds a search entry.
    pub fn add_search_entry(&mut self, entry: SearchEntry) {
        self.index.search.entries.push(entry);
    }

    /// Exports the DocGraph as JSON.
    pub fn to_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(self)
    }

    /// Exports the DocGraph as compact JSON.
    pub fn to_json_compact(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_docgraph_creation() {
        let graph = DocGraph::new("test-project".to_string(), "0.1.0".to_string());
        assert_eq!(graph.meta.schema.name, "kraken-docgraph");
        assert_eq!(graph.meta.schema.version, 1);
        assert_eq!(graph.meta.source.project, "test-project");
    }

    #[test]
    fn test_add_node() {
        let mut graph = DocGraph::new("test".to_string(), "0.1.0".to_string());

        let node = Node {
            id: "test-fn".to_string(),
            kind: NodeKind::Symbol,
            title: "test_function".to_string(),
            stability: Stability::Stable,
            qualified_name: Some("module::test_function".to_string()),
            since: Some("0.1.0".to_string()),
            deprecated: None,
            tags: Some(vec!["testing".to_string()]),
            source_span: None,
            docs: None,
            module: None,
            symbol: Some(SymbolNode {
                kind: "fn".to_string(),
                module_id: Some("module".to_string()),
                signature: None,
                visibility: Some("public".to_string()),
                type_id: None,
            }),
        };

        graph.add_node(node);
        assert_eq!(graph.nodes.len(), 1);
        assert!(graph.index.node_kinds.contains(&"symbol".to_string()));
        assert!(graph.index.tags.contains(&"testing".to_string()));
    }

    #[test]
    fn test_add_page() {
        let mut graph = DocGraph::new("test".to_string(), "0.1.0".to_string());

        let page = Page {
            id: "intro".to_string(),
            slug: "introduction".to_string(),
            title: "Introduction".to_string(),
            summary: Some("Getting started".to_string()),
            tags: None,
            sections: vec![PageSection {
                kind: "markdown".to_string(),
                title: Some("Overview".to_string()),
                markdown: Some("# Overview\n\nWelcome!".to_string()),
                node_id: None,
                node_ids: None,
            }],
        };

        graph.add_page(page);
        assert_eq!(graph.pages.len(), 1);
    }

    #[test]
    fn test_add_search_entry() {
        let mut graph = DocGraph::new("test".to_string(), "0.1.0".to_string());

        let entry = SearchEntry {
            node_id: "test-fn".to_string(),
            title: "test_function".to_string(),
            kind: "function".to_string(),
            path: "module::test_function".to_string(),
            tokens: vec!["test".to_string(), "function".to_string()],
            boost: 1.0,
        };

        graph.add_search_entry(entry);
        assert_eq!(graph.index.search.entries.len(), 1);
    }

    #[test]
    fn test_json_export() {
        let graph = DocGraph::new("test".to_string(), "0.1.0".to_string());
        let json = graph.to_json().unwrap();
        assert!(json.contains("kraken-docgraph"));
        assert!(json.contains("test"));
    }

    #[test]
    fn test_stability_serialization() {
        let node = Node {
            id: "test".to_string(),
            kind: NodeKind::Symbol,
            title: "test".to_string(),
            stability: Stability::Experimental,
            qualified_name: None,
            since: None,
            deprecated: None,
            tags: None,
            source_span: None,
            docs: None,
            module: None,
            symbol: None,
        };

        let json = serde_json::to_string(&node).unwrap();
        assert!(json.contains("experimental"));
    }

    #[test]
    fn test_type_ref() {
        let type_ref = TypeRef {
            display: "Vec<T>".to_string(),
            node_id: Some("vec".to_string()),
            args: Some(vec![TypeRef {
                display: "T".to_string(),
                node_id: None,
                args: None,
            }]),
        };

        let json = serde_json::to_string(&type_ref).unwrap();
        assert!(json.contains("Vec<T>"));
    }

    #[test]
    fn test_signature() {
        let sig = Signature {
            text: "fn test(x: int) -> bool".to_string(),
            params: Some(vec![Param {
                name: "x".to_string(),
                type_ref: TypeRef {
                    display: "int".to_string(),
                    node_id: None,
                    args: None,
                },
                default: None,
                variadic: false,
            }]),
            returns: Some(TypeRef {
                display: "bool".to_string(),
                node_id: None,
                args: None,
            }),
            generics: None,
        };

        let json = serde_json::to_string(&sig).unwrap();
        assert!(json.contains("test"));
        assert!(json.contains("int"));
    }
}
