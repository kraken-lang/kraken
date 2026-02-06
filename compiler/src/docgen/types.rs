//! DocGraph type definitions — serializable structures for JSON output.

use serde::Serialize;
use std::collections::BTreeMap;

#[derive(Debug, Serialize)]
pub struct DocGraph {
    pub meta: Meta,
    pub index: Index,
    pub pages: Vec<Page>,
    pub nodes: BTreeMap<String, Node>,
}

#[derive(Debug, Serialize)]
pub struct Meta {
    pub schema: SchemaRef,
    pub generated_at: String,
    pub tool: ToolRef,
    pub source: SourceRef,
}

#[derive(Debug, Serialize)]
pub struct SchemaRef {
    pub name: &'static str,
    pub version: u32,
}

#[derive(Debug, Serialize)]
pub struct ToolRef {
    pub name: String,
    pub version: String,
}

#[derive(Debug, Serialize)]
pub struct SourceRef {
    pub project: String,
    pub revision: String,
}

#[derive(Debug, Serialize)]
pub struct Index {
    pub node_kinds: Vec<String>,
    pub tags: Vec<String>,
    pub search: SearchIndex,
}

#[derive(Debug, Serialize)]
pub struct SearchIndex {
    pub entries: Vec<SearchEntry>,
}

#[derive(Debug, Serialize)]
pub struct SearchEntry {
    pub node_id: String,
    pub title: String,
    pub kind: String,
    pub path: String,
    pub tokens: Vec<String>,
}

#[derive(Debug, Serialize)]
pub struct Page {
    pub id: String,
    pub slug: String,
    pub title: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub summary: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub nav: Option<Nav>,
    pub sections: Vec<PageSection>,
}

#[derive(Debug, Serialize)]
pub struct Nav {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub group: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub order: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parent_page_id: Option<String>,
}

#[derive(Debug, Serialize)]
pub struct PageSection {
    pub kind: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub markdown: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub node_ids: Option<Vec<String>>,
}

#[derive(Debug, Serialize)]
pub struct Node {
    pub id: String,
    pub kind: String,
    pub title: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub qualified_name: Option<String>,
    pub stability: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub since: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tags: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub docs: Option<DocBlock>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub symbol: Option<SymbolNode>,
    #[serde(rename = "type", skip_serializing_if = "Option::is_none")]
    pub type_node: Option<TypeNode>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub diagnostic: Option<DiagnosticNode>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub concept: Option<ConceptNode>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tooling: Option<ToolingNode>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub links: Option<Links>,
}

#[derive(Debug, Serialize)]
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

#[derive(Debug, Serialize)]
pub struct Example {
    pub title: String,
    pub code: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub language: Option<String>,
}

#[derive(Debug, Serialize)]
pub struct SymbolNode {
    pub kind: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub signature: Option<Signature>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub visibility: Option<String>,
}

#[derive(Debug, Serialize)]
pub struct Signature {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub text: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub params: Option<Vec<Param>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub returns: Option<TypeRef>,
}

#[derive(Debug, Serialize)]
pub struct Param {
    pub name: String,
    #[serde(rename = "type")]
    pub param_type: TypeRef,
}

#[derive(Debug, Serialize)]
pub struct TypeRef {
    pub display: String,
}

#[derive(Debug, Serialize)]
pub struct TypeNode {
    pub kind: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fields: Option<Vec<Field>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub variants: Option<Vec<Variant>>,
}

#[derive(Debug, Serialize)]
pub struct Field {
    pub name: String,
    #[serde(rename = "type")]
    pub field_type: TypeRef,
}

#[derive(Debug, Serialize)]
pub struct Variant {
    pub name: String,
}

#[derive(Debug, Serialize)]
pub struct DiagnosticNode {
    pub code: String,
    pub severity: String,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub category: Option<String>,
}

#[derive(Debug, Serialize)]
pub struct ConceptNode {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub group: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub keywords: Option<Vec<String>>,
}

#[derive(Debug, Serialize)]
pub struct ToolingNode {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub commands: Option<Vec<ToolCommand>>,
}

#[derive(Debug, Serialize)]
pub struct ToolCommand {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub summary: Option<String>,
}

#[derive(Debug, Serialize)]
pub struct Links {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub related: Option<Vec<String>>,
}
