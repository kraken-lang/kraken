//! Doc command - Generate documentation from source code.
//!
//! Supports: doc comment extraction (`///`, `//!`), Markdown rendering in comments,
//! cross-references via `[`link`]` syntax, fenced code block examples, a client-side
//! search index, and DocGraph JSON metadata generation.

use crate::cli::output::{OutputMessage, ProgressIndicator};
use crate::cli::{Command, CommandResult};
use crate::docgen;
use glob::glob;
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

// ---------------------------------------------------------------------------
// Structured doc-comment data extracted from a single source file
// ---------------------------------------------------------------------------

/// A documentation block attached to a named declaration.
#[derive(Debug, Clone)]
struct DocItem {
    name: String,
    kind: &'static str,
    lines: Vec<String>,
    line_number: usize,
}

/// All documentation extracted from one source file.
#[derive(Debug)]
struct FileDoc {
    path: PathBuf,
    module_docs: Vec<String>,
    items: Vec<DocItem>,
}

// ---------------------------------------------------------------------------
// DocCommand
// ---------------------------------------------------------------------------

/// Doc command: generates HTML documentation, a search index, and DocGraph JSON.
pub struct DocCommand {
    #[allow(dead_code)]
    open: bool,
    output_dir: PathBuf,
}

impl DocCommand {
    /// Create a new doc command with default output directory (`docs/`).
    pub fn create() -> Box<dyn Command> {
        Box::new(Self {
            open: false,
            output_dir: PathBuf::from("docs"),
        })
    }

    // -- Source discovery ---------------------------------------------------

    fn discover_source_files(&self, root: &Path) -> Vec<PathBuf> {
        let mut files = Vec::new();
        let pattern = format!("{}/**/*.kr", root.display());
        if let Ok(entries) = glob(&pattern) {
            for entry in entries.flatten() {
                files.push(entry);
            }
        }
        files
    }

    // -- Doc comment extraction (structured) --------------------------------

    fn extract_file_doc(&self, path: &Path, source: &str) -> FileDoc {
        let mut module_docs = Vec::new();
        let mut items: Vec<DocItem> = Vec::new();
        let mut pending_lines: Vec<String> = Vec::new();
        let mut pending_start: usize = 0;

        for (idx, line) in source.lines().enumerate() {
            let trimmed = line.trim();

            // Module doc comments (//!)
            if let Some(stripped) = trimmed.strip_prefix("//!") {
                module_docs.push(stripped.trim().to_string());
                continue;
            }

            // Item doc comments (///)
            if let Some(stripped) = trimmed.strip_prefix("///") {
                if pending_lines.is_empty() {
                    pending_start = idx + 1;
                }
                pending_lines.push(stripped.trim().to_string());
                continue;
            }

            // After doc comments, look for a declaration to attach them to
            if !pending_lines.is_empty() {
                let (name, kind) = Self::detect_declaration(trimmed);
                items.push(DocItem {
                    name,
                    kind,
                    lines: std::mem::take(&mut pending_lines),
                    line_number: pending_start,
                });
            }
        }

        // Trailing doc comments without a following declaration
        if !pending_lines.is_empty() {
            items.push(DocItem {
                name: "(trailing)".into(),
                kind: "unknown",
                lines: pending_lines,
                line_number: pending_start,
            });
        }

        FileDoc {
            path: path.to_path_buf(),
            module_docs,
            items,
        }
    }

    /// Detect what kind of declaration follows a doc comment block.
    fn detect_declaration(line: &str) -> (String, &'static str) {
        let tokens: Vec<&str> = line.split_whitespace().collect();
        // Skip visibility modifier
        let start = if tokens.first() == Some(&"pub") { 1 } else { 0 };
        let kw = tokens.get(start).copied().unwrap_or("");
        let name_pos = start + 1;
        let raw = tokens.get(name_pos).unwrap_or(&"(anonymous)");
        let name = raw
            .split(&['(', '<', '{'][..])
            .next()
            .unwrap_or(raw)
            .to_string();

        let kind = match kw {
            "fn" | "async" => "function",
            "struct" => "struct",
            "enum" => "enum",
            "trait" => "trait",
            "impl" => "impl",
            "type" => "type_alias",
            "const" => "constant",
            "let" => "variable",
            "module" => "module",
            "import" => "import",
            "class" => "class",
            "interface" => "interface",
            "union" => "union",
            "macro_rules" => "macro",
            _ => "unknown",
        };
        (name, kind)
    }

    // -- Markdown-lite rendering --------------------------------------------

    /// Convert doc comment lines to HTML with basic Markdown support.
    fn render_markdown(lines: &[String], all_names: &[String]) -> String {
        let mut html = String::new();
        let mut in_code_block = false;
        let mut code_lang = String::new();
        let mut code_buf = String::new();

        for line in lines {
            // Fenced code blocks
            if line.starts_with("```") {
                if in_code_block {
                    html.push_str(&format!(
                        "<pre><code class=\"language-{code_lang}\">{}</code></pre>\n",
                        Self::escape_html(&code_buf)
                    ));
                    code_buf.clear();
                    in_code_block = false;
                } else {
                    code_lang = line.trim_start_matches('`').to_string();
                    if code_lang.is_empty() {
                        code_lang = "kraken".into();
                    }
                    in_code_block = true;
                }
                continue;
            }
            if in_code_block {
                if !code_buf.is_empty() {
                    code_buf.push('\n');
                }
                code_buf.push_str(line);
                continue;
            }

            // Headers
            if let Some(h) = line.strip_prefix("### ") {
                html.push_str(&format!("<h4>{}</h4>\n", Self::escape_html(h)));
                continue;
            }
            if let Some(h) = line.strip_prefix("## ") {
                html.push_str(&format!("<h3>{}</h3>\n", Self::escape_html(h)));
                continue;
            }
            if let Some(h) = line.strip_prefix("# ") {
                html.push_str(&format!("<h2>{}</h2>\n", Self::escape_html(h)));
                continue;
            }

            // Empty line = paragraph break
            if line.is_empty() {
                html.push_str("<br>\n");
                continue;
            }

            // Inline formatting + cross-references
            let processed = Self::process_inline(line, all_names);
            html.push_str(&format!("<p>{processed}</p>\n"));
        }

        // Unclosed code block
        if in_code_block && !code_buf.is_empty() {
            html.push_str(&format!(
                "<pre><code class=\"language-{code_lang}\">{}</code></pre>\n",
                Self::escape_html(&code_buf)
            ));
        }

        html
    }

    /// Process inline Markdown: `code`, **bold**, *italic*, [`cross-ref`].
    fn process_inline(line: &str, all_names: &[String]) -> String {
        let escaped = Self::escape_html(line);
        let mut result = String::with_capacity(escaped.len());
        let chars: Vec<char> = escaped.chars().collect();
        let len = chars.len();
        let mut i = 0;

        while i < len {
            // Cross-reference: [`name`]
            if i + 1 < len && chars[i] == '[' && chars[i + 1] == '`' {
                if let Some(end) = Self::find_cross_ref(&chars, i) {
                    let name: String = chars[i + 2..end].iter().collect();
                    let anchor = name.replace("::", "-").to_lowercase();
                    let is_known = all_names.iter().any(|n| n == &name);
                    if is_known {
                        result.push_str(&format!(
                            "<a href=\"#{anchor}\" class=\"xref\"><code>{name}</code></a>"
                        ));
                    } else {
                        result.push_str(&format!("<code>{name}</code>"));
                    }
                    i = end + 2; // skip past `]
                    continue;
                }
            }
            // Inline code: `code`
            if chars[i] == '`' {
                if let Some(end) = chars[i + 1..].iter().position(|&c| c == '`') {
                    let code: String = chars[i + 1..i + 1 + end].iter().collect();
                    result.push_str(&format!("<code>{code}</code>"));
                    i = i + 2 + end;
                    continue;
                }
            }
            // Bold: **text**
            if i + 1 < len && chars[i] == '*' && chars[i + 1] == '*' {
                if let Some(end) = Self::find_double(&chars, i + 2, '*') {
                    let text: String = chars[i + 2..end].iter().collect();
                    result.push_str(&format!("<strong>{text}</strong>"));
                    i = end + 2;
                    continue;
                }
            }
            // Italic: *text*
            if chars[i] == '*' {
                if let Some(end) = chars[i + 1..].iter().position(|&c| c == '*') {
                    let text: String = chars[i + 1..i + 1 + end].iter().collect();
                    result.push_str(&format!("<em>{text}</em>"));
                    i = i + 2 + end;
                    continue;
                }
            }
            result.push(chars[i]);
            i += 1;
        }
        result
    }

    fn find_cross_ref(chars: &[char], start: usize) -> Option<usize> {
        // Looking for [`...`] starting at `start`
        let mut i = start + 2; // skip [`
        while i < chars.len() {
            if chars[i] == '`' && i + 1 < chars.len() && chars[i + 1] == ']' {
                return Some(i);
            }
            i += 1;
        }
        None
    }

    fn find_double(chars: &[char], start: usize, ch: char) -> Option<usize> {
        let mut i = start;
        while i + 1 < chars.len() {
            if chars[i] == ch && chars[i + 1] == ch {
                return Some(i);
            }
            i += 1;
        }
        None
    }

    fn escape_html(s: &str) -> String {
        s.replace('&', "&amp;")
            .replace('<', "&lt;")
            .replace('>', "&gt;")
            .replace('"', "&quot;")
    }

    // -- HTML generation (enhanced) -----------------------------------------

    fn generate_html(&self, file_doc: &FileDoc, all_names: &[String]) -> String {
        let file_name = file_doc.path.file_name().unwrap().to_string_lossy();

        let mut html = format!(
            r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{file_name} — Kraken Documentation</title>
    <style>
        :root {{ --bg: #ffffff; --fg: #1a1a2e; --accent: #0f3460; --code-bg: #f0f0f5;
                --border: #e0e0e0; --link: #0969da; --block-bg: #f6f8fa; }}
        * {{ box-sizing: border-box; margin: 0; padding: 0; }}
        body {{ font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
               max-width: 960px; margin: 0 auto; padding: 2rem; color: var(--fg);
               line-height: 1.6; background: var(--bg); }}
        h1 {{ color: var(--accent); border-bottom: 2px solid var(--border); padding-bottom: .5rem;
              margin-bottom: 1.5rem; font-size: 1.75rem; }}
        h2 {{ margin-top: 2rem; color: var(--accent); font-size: 1.35rem; }}
        h3 {{ margin-top: 1.5rem; font-size: 1.1rem; }}
        a {{ color: var(--link); text-decoration: none; }}
        a:hover {{ text-decoration: underline; }}
        code {{ background: var(--code-bg); padding: 2px 6px; border-radius: 3px;
               font-size: 0.9em; }}
        pre {{ background: #1e1e2e; color: #cdd6f4; padding: 1rem; border-radius: 6px;
              overflow-x: auto; margin: .75rem 0; }}
        pre code {{ background: none; padding: 0; }}
        .doc-item {{ background: var(--block-bg); padding: 1rem 1.25rem; margin: 1rem 0;
                    border-left: 4px solid var(--accent); border-radius: 0 6px 6px 0; }}
        .doc-item h3 {{ margin-top: 0; }}
        .badge {{ display: inline-block; font-size: .75rem; padding: 2px 8px;
                 border-radius: 12px; background: var(--accent); color: #fff;
                 margin-left: .5rem; vertical-align: middle; }}
        .module-doc {{ background: #eef2ff; padding: 1rem; border-radius: 6px;
                      margin-bottom: 1.5rem; }}
        .search-box {{ margin-bottom: 1.5rem; }}
        .search-box input {{ width: 100%; padding: .5rem .75rem; border: 1px solid var(--border);
                            border-radius: 6px; font-size: 1rem; }}
        .xref {{ border-bottom: 1px dashed var(--link); }}
        .breadcrumb {{ font-size: .875rem; color: #666; margin-bottom: 1rem; }}
        .breadcrumb a {{ color: #666; }}
        .toc {{ background: var(--block-bg); padding: 1rem; border-radius: 6px;
               margin-bottom: 1.5rem; }}
        .toc ul {{ list-style: none; padding-left: 1rem; }}
        .toc li {{ margin: .25rem 0; }}
        .hidden {{ display: none; }}
    </style>
</head>
<body>
    <nav class="breadcrumb"><a href="index.html">Index</a> &rsaquo; {file_name}</nav>
    <h1>{file_name}</h1>
    <div class="search-box">
        <input type="text" id="search" placeholder="Search declarations..." oninput="filterItems()">
    </div>
"#
        );

        // Module-level docs
        if !file_doc.module_docs.is_empty() {
            let rendered = Self::render_markdown(&file_doc.module_docs, all_names);
            html.push_str(&format!("<div class=\"module-doc\">{rendered}</div>\n"));
        }

        // Table of contents
        if !file_doc.items.is_empty() {
            html.push_str("<nav class=\"toc\"><strong>Contents</strong><ul>\n");
            for item in &file_doc.items {
                let anchor = item.name.to_lowercase().replace("::", "-");
                html.push_str(&format!(
                    "<li><a href=\"#{anchor}\">{}</a> <span class=\"badge\">{}</span></li>\n",
                    Self::escape_html(&item.name),
                    item.kind
                ));
            }
            html.push_str("</ul></nav>\n");
        }

        // Doc items
        if file_doc.items.is_empty() && file_doc.module_docs.is_empty() {
            html.push_str("<p><em>No documentation comments found.</em></p>");
        } else {
            for item in &file_doc.items {
                let anchor = item.name.to_lowercase().replace("::", "-");
                let rendered = Self::render_markdown(&item.lines, all_names);
                html.push_str(&format!(
                    "<div class=\"doc-item\" id=\"{anchor}\" data-name=\"{}\">\
                     <h3>{} <span class=\"badge\">{}</span> \
                     <small style=\"color:#999\">line {}</small></h3>\n\
                     {rendered}</div>\n",
                    Self::escape_html(&item.name.to_lowercase()),
                    Self::escape_html(&item.name),
                    item.kind,
                    item.line_number
                ));
            }
        }

        // Search script
        html.push_str(
            r#"
    <script>
    function filterItems() {
        const q = document.getElementById('search').value.toLowerCase();
        document.querySelectorAll('.doc-item').forEach(el => {
            const name = el.getAttribute('data-name') || '';
            el.classList.toggle('hidden', q.length > 0 && !name.includes(q));
        });
    }
    </script>
</body>
</html>
"#,
        );

        html
    }

    fn generate_index(&self, file_docs: &[FileDoc]) -> String {
        let mut html = r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Kraken Documentation</title>
    <style>
        :root { --bg: #ffffff; --fg: #1a1a2e; --accent: #0f3460; --border: #e0e0e0;
                --link: #0969da; --block-bg: #f6f8fa; }
        * { box-sizing: border-box; margin: 0; padding: 0; }
        body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
               max-width: 960px; margin: 0 auto; padding: 2rem; color: var(--fg); line-height: 1.6; }
        h1 { color: var(--accent); margin-bottom: 1.5rem; }
        h2 { color: var(--accent); margin-top: 1.5rem; margin-bottom: .75rem; }
        a { color: var(--link); text-decoration: none; }
        a:hover { text-decoration: underline; }
        .search-box { margin-bottom: 1.5rem; }
        .search-box input { width: 100%; padding: .5rem .75rem; border: 1px solid var(--border);
                            border-radius: 6px; font-size: 1rem; }
        .file-list { list-style: none; padding: 0; }
        .file-list li { padding: .75rem 1rem; margin: .5rem 0; background: var(--block-bg);
                       border-radius: 6px; display: flex; justify-content: space-between; }
        .badge { font-size: .75rem; padding: 2px 8px; border-radius: 12px;
                background: var(--accent); color: #fff; }
        .stats { color: #666; font-size: .875rem; margin-bottom: 1.5rem; }
        .hidden { display: none; }
    </style>
</head>
<body>
    <h1>Kraken Documentation</h1>
"#
        .to_string();

        // Stats
        let total_items: usize = file_docs.iter().map(|f| f.items.len()).sum();
        html.push_str(&format!(
            "<p class=\"stats\">{} source files &middot; {} documented items</p>\n",
            file_docs.len(),
            total_items
        ));

        // Search
        html.push_str(
            "<div class=\"search-box\">\
             <input type=\"text\" id=\"search\" placeholder=\"Search files...\" oninput=\"filterFiles()\">\
             </div>\n",
        );

        // File list
        html.push_str("<h2>Source Files</h2>\n<ul class=\"file-list\">\n");
        for fd in file_docs {
            let file_name = fd.path.file_name().unwrap().to_string_lossy();
            let html_name = format!("{file_name}.html");
            let count = fd.items.len();
            html.push_str(&format!(
                "<li data-name=\"{}\"><a href=\"{html_name}\">{file_name}</a> \
                 <span class=\"badge\">{count} items</span></li>\n",
                file_name.to_lowercase()
            ));
        }
        html.push_str("</ul>\n");

        // Link to API metadata
        html.push_str(
            "<h2>API Reference</h2>\n\
             <p><a href=\"generated/docgraph.json\">DocGraph JSON</a> &middot; \
             <a href=\"generated/search_index.json\">Search Index</a></p>\n",
        );

        // Search script
        html.push_str(
            r#"
    <script>
    function filterFiles() {
        const q = document.getElementById('search').value.toLowerCase();
        document.querySelectorAll('.file-list li').forEach(el => {
            const name = el.getAttribute('data-name') || '';
            el.classList.toggle('hidden', q.length > 0 && !name.includes(q));
        });
    }
    </script>
</body>
</html>
"#,
        );

        html
    }

    // -- DocGraph JSON generation -------------------------------------------

    fn generate_docgraph_json(&self) -> Result<(), String> {
        let out_dir = self.output_dir.join("generated");
        fs::create_dir_all(&out_dir).map_err(|e| format!("Failed to create generated dir: {e}"))?;

        let graph = docgen::generate();

        // Full DocGraph
        let json = serde_json::to_string_pretty(&graph)
            .map_err(|e| format!("JSON serialization failed: {e}"))?;
        fs::write(out_dir.join("docgraph.json"), &json)
            .map_err(|e| format!("Failed to write docgraph.json: {e}"))?;

        // Standalone search index
        let search_json = serde_json::to_string_pretty(&graph.index.search)
            .map_err(|e| format!("Search index serialization failed: {e}"))?;
        fs::write(out_dir.join("search_index.json"), &search_json)
            .map_err(|e| format!("Failed to write search_index.json: {e}"))?;

        // Per-page JSON files
        for page in &graph.pages {
            let page_json = serde_json::to_string_pretty(page)
                .map_err(|e| format!("Page serialization failed: {e}"))?;
            fs::write(out_dir.join(format!("{}.json", page.slug)), &page_json)
                .map_err(|e| format!("Failed to write {}.json: {e}", page.slug))?;
        }

        // Link index: node_id -> { title, kind, page }
        let link_index: BTreeMap<&str, serde_json::Value> = graph
            .index
            .search
            .entries
            .iter()
            .map(|e| {
                (
                    e.node_id.as_str(),
                    serde_json::json!({
                        "title": e.title,
                        "kind": e.kind,
                        "path": e.path
                    }),
                )
            })
            .collect();
        let link_json = serde_json::to_string_pretty(&link_index)
            .map_err(|e| format!("Link index serialization failed: {e}"))?;
        fs::write(out_dir.join("link_index.json"), &link_json)
            .map_err(|e| format!("Failed to write link_index.json: {e}"))?;

        Ok(())
    }
}

impl Command for DocCommand {
    fn name(&self) -> &str {
        "doc"
    }

    fn description(&self) -> &str {
        "Generate documentation from source code"
    }

    fn execute(&self, _args: Vec<String>) -> CommandResult {
        let project_root =
            std::env::current_dir().map_err(|e| format!("Failed to get current directory: {e}"))?;

        println!("{}", OutputMessage::info("Discovering source files"));

        let files = self.discover_source_files(&project_root);

        if files.is_empty() {
            println!("{}", OutputMessage::warning("No source files found"));
            // Still generate DocGraph metadata even without source files
            self.generate_docgraph_json()?;
            println!(
                "{}",
                OutputMessage::success("DocGraph metadata generated (no source files)")
            );
            return Ok(());
        }

        println!(
            "{}",
            OutputMessage::info(format!(
                "Generating documentation for {} files",
                files.len()
            ))
        );

        // Create output directory
        fs::create_dir_all(&self.output_dir)
            .map_err(|e| format!("Failed to create output directory: {e}"))?;

        // Extract structured docs from all files
        let mut file_docs: Vec<FileDoc> = Vec::with_capacity(files.len());
        for file in &files {
            let source = fs::read_to_string(file)
                .map_err(|e| format!("Failed to read {}: {e}", file.display()))?;
            file_docs.push(self.extract_file_doc(file, &source));
        }

        // Collect all declaration names for cross-referencing
        let all_names: Vec<String> = file_docs
            .iter()
            .flat_map(|fd| fd.items.iter().map(|item| item.name.clone()))
            .collect();

        let progress = ProgressIndicator::new("Generating docs", files.len() as u64);

        for (i, file_doc) in file_docs.iter().enumerate() {
            progress.update((i + 1) as u64);

            let html = self.generate_html(file_doc, &all_names);

            let file_name = file_doc.path.file_name().unwrap().to_string_lossy();
            let output_path = self.output_dir.join(format!("{file_name}.html"));

            fs::write(&output_path, html)
                .map_err(|e| format!("Failed to write {}: {e}", output_path.display()))?;
        }

        // Generate index
        let index_html = self.generate_index(&file_docs);
        fs::write(self.output_dir.join("index.html"), index_html)
            .map_err(|e| format!("Failed to write index.html: {e}"))?;

        progress.finish("Documentation generated");

        // Generate DocGraph JSON metadata
        println!("{}", OutputMessage::info("Generating DocGraph metadata"));
        self.generate_docgraph_json()?;

        // Generate LSIF dump
        println!("{}", OutputMessage::info("Generating LSIF index"));
        self.generate_lsif_dump(&project_root)?;

        println!(
            "{}",
            OutputMessage::success(format!(
                "Documentation generated in {}",
                self.output_dir.display()
            ))
        );

        Ok(())
    }
}

impl DocCommand {
    /// Generate a `dump.lsif` file at the project root.
    fn generate_lsif_dump(&self, project_root: &Path) -> Result<(), String> {
        let graph = docgen::generate();
        let lsif = docgen::lsif::generate_lsif(project_root, &graph);
        let dump_path = project_root.join("dump.lsif");
        fs::write(&dump_path, &lsif).map_err(|e| format!("Failed to write dump.lsif: {e}"))?;
        println!(
            "{}",
            OutputMessage::info(format!(
                "LSIF dump written to {} ({} bytes)",
                dump_path.display(),
                lsif.len()
            ))
        );
        Ok(())
    }
}

#[allow(clippy::derivable_impls)]
impl Default for DocCommand {
    fn default() -> Self {
        Self {
            open: false,
            output_dir: PathBuf::from("docs"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_doc_command() {
        let cmd = DocCommand::create();
        assert_eq!(cmd.name(), "doc");
    }

    #[test]
    fn test_extract_module_docs() {
        let cmd = DocCommand::default();
        let source = "//! Module documentation\n//! Second line\nfn main() {}\n";
        let fd = cmd.extract_file_doc(Path::new("test.kr"), source);
        assert_eq!(fd.module_docs.len(), 2);
        assert_eq!(fd.module_docs[0], "Module documentation");
    }

    #[test]
    fn test_extract_item_docs() {
        let cmd = DocCommand::default();
        let source = "/// Adds two numbers.\n/// Returns the sum.\nfn add(a: int, b: int) -> int {\n    return a + b;\n}\n";
        let fd = cmd.extract_file_doc(Path::new("test.kr"), source);
        assert_eq!(fd.items.len(), 1);
        assert_eq!(fd.items[0].name, "add");
        assert_eq!(fd.items[0].kind, "function");
        assert_eq!(fd.items[0].lines.len(), 2);
    }

    #[test]
    fn test_extract_struct_docs() {
        let cmd = DocCommand::default();
        let source = "/// A 2D point.\npub struct Point {\n    x: int;\n    y: int;\n}\n";
        let fd = cmd.extract_file_doc(Path::new("test.kr"), source);
        assert_eq!(fd.items.len(), 1);
        assert_eq!(fd.items[0].name, "Point");
        assert_eq!(fd.items[0].kind, "struct");
    }

    #[test]
    fn test_extract_enum_docs() {
        let cmd = DocCommand::default();
        let source = "/// Color variants.\nenum Color {\n    Red,\n    Green,\n}\n";
        let fd = cmd.extract_file_doc(Path::new("test.kr"), source);
        assert_eq!(fd.items.len(), 1);
        assert_eq!(fd.items[0].name, "Color");
        assert_eq!(fd.items[0].kind, "enum");
    }

    #[test]
    fn test_render_markdown_code_block() {
        let lines = vec![
            "Example:".into(),
            "```kraken".into(),
            "let x = 42;".into(),
            "```".into(),
        ];
        let html = DocCommand::render_markdown(&lines, &[]);
        assert!(html.contains("<pre>"), "should have pre block");
        assert!(html.contains("let x = 42;"), "should have code content");
    }

    #[test]
    fn test_render_markdown_inline_code() {
        let lines = vec!["Use `printf` to print.".into()];
        let html = DocCommand::render_markdown(&lines, &[]);
        assert!(
            html.contains("<code>printf</code>"),
            "should render inline code"
        );
    }

    #[test]
    fn test_render_markdown_bold() {
        let lines = vec!["This is **important** text.".into()];
        let html = DocCommand::render_markdown(&lines, &[]);
        assert!(
            html.contains("<strong>important</strong>"),
            "should render bold"
        );
    }

    #[test]
    fn test_render_markdown_headers() {
        let lines = vec!["# Title".into(), "## Section".into()];
        let html = DocCommand::render_markdown(&lines, &[]);
        assert!(html.contains("<h2>Title</h2>"), "should render h1 as h2");
        assert!(html.contains("<h3>Section</h3>"), "should render h2 as h3");
    }

    #[test]
    fn test_cross_reference_known_name() {
        let lines = vec!["See [`Point`] for details.".into()];
        let names = vec!["Point".to_string()];
        let html = DocCommand::render_markdown(&lines, &names);
        assert!(html.contains("href=\"#point\""), "should link to anchor");
        assert!(html.contains("class=\"xref\""), "should have xref class");
    }

    #[test]
    fn test_cross_reference_unknown_name() {
        let lines = vec!["See [`Unknown`] for details.".into()];
        let html = DocCommand::render_markdown(&lines, &[]);
        assert!(
            !html.contains("href="),
            "should not create link for unknown"
        );
        assert!(
            html.contains("<code>Unknown</code>"),
            "should render as code"
        );
    }

    #[test]
    fn test_detect_declaration_fn() {
        let (name, kind) = DocCommand::detect_declaration("fn hello() -> void {");
        assert_eq!(name, "hello");
        assert_eq!(kind, "function");
    }

    #[test]
    fn test_detect_declaration_pub_struct() {
        let (name, kind) = DocCommand::detect_declaration("pub struct MyType {");
        assert_eq!(name, "MyType");
        assert_eq!(kind, "struct");
    }

    #[test]
    fn test_detect_declaration_trait() {
        let (name, kind) = DocCommand::detect_declaration("trait Drawable {");
        assert_eq!(name, "Drawable");
        assert_eq!(kind, "trait");
    }

    #[test]
    fn test_detect_declaration_generic() {
        let (name, kind) = DocCommand::detect_declaration("fn identity<T>(x: T) -> T {");
        assert_eq!(name, "identity");
        assert_eq!(kind, "function");
    }

    #[test]
    fn test_escape_html() {
        assert_eq!(DocCommand::escape_html("<b>"), "&lt;b&gt;");
        assert_eq!(DocCommand::escape_html("a&b"), "a&amp;b");
    }

    #[test]
    fn test_html_generation_has_search() {
        let cmd = DocCommand::default();
        let source = "/// A test function.\nfn test_fn() {}\n";
        let fd = cmd.extract_file_doc(Path::new("test.kr"), source);
        let html = cmd.generate_html(&fd, &["test_fn".to_string()]);
        assert!(html.contains("id=\"search\""), "should have search input");
        assert!(html.contains("filterItems"), "should have filter script");
    }

    #[test]
    fn test_html_generation_has_toc() {
        let cmd = DocCommand::default();
        let source = "/// First.\nfn a() {}\n/// Second.\nfn b() {}\n";
        let fd = cmd.extract_file_doc(Path::new("test.kr"), source);
        let html = cmd.generate_html(&fd, &[]);
        assert!(
            html.contains("class=\"toc\""),
            "should have table of contents"
        );
    }

    #[test]
    fn test_doc_default() {
        let cmd = DocCommand::default();
        assert!(!cmd.open);
        assert_eq!(cmd.output_dir, PathBuf::from("docs"));
    }

    #[test]
    fn test_trailing_doc_comments() {
        let cmd = DocCommand::default();
        let source = "/// Trailing comment\n";
        let fd = cmd.extract_file_doc(Path::new("test.kr"), source);
        assert_eq!(fd.items.len(), 1);
        assert_eq!(fd.items[0].name, "(trailing)");
        assert_eq!(fd.items[0].kind, "unknown");
    }

    #[test]
    fn test_detect_declaration_all_keywords() {
        for (line, expected_kind) in [
            ("enum Foo {", "enum"),
            ("trait Bar {", "trait"),
            ("impl Baz {", "impl"),
            ("type Alias = int;", "type_alias"),
            ("const MAX = 100;", "constant"),
            ("let x = 5;", "variable"),
            ("module foo;", "module"),
            ("import std;", "import"),
            ("class Widget {", "class"),
            ("interface Drawable {", "interface"),
            ("union Value {", "union"),
            ("macro_rules my_macro {", "macro"),
            ("async fn run() {", "function"),
            ("something_else;", "unknown"),
        ] {
            let (_, kind) = DocCommand::detect_declaration(line);
            assert_eq!(kind, expected_kind, "Failed for: {line}");
        }
    }

    #[test]
    fn test_render_markdown_italic() {
        let lines = vec!["This is *italic* text.".into()];
        let html = DocCommand::render_markdown(&lines, &[]);
        assert!(html.contains("<em>italic</em>"));
    }

    #[test]
    fn test_render_markdown_empty_line() {
        let lines = vec!["First.".into(), "".into(), "Second.".into()];
        let html = DocCommand::render_markdown(&lines, &[]);
        assert!(html.contains("<br>"));
    }

    #[test]
    fn test_render_markdown_h3() {
        let lines = vec!["### Subsection".into()];
        let html = DocCommand::render_markdown(&lines, &[]);
        assert!(html.contains("<h4>Subsection</h4>"));
    }

    #[test]
    fn test_render_markdown_unclosed_code_block() {
        let lines = vec!["```".into(), "let x = 1;".into()];
        let html = DocCommand::render_markdown(&lines, &[]);
        assert!(html.contains("<pre>"));
        assert!(html.contains("let x = 1;"));
    }

    #[test]
    fn test_render_markdown_code_block_no_lang() {
        let lines = vec!["```".into(), "code".into(), "```".into()];
        let html = DocCommand::render_markdown(&lines, &[]);
        assert!(html.contains("language-kraken"));
    }

    #[test]
    fn test_discover_source_files_empty() {
        let cmd = DocCommand::default();
        let files = cmd.discover_source_files(Path::new("/nonexistent"));
        assert!(files.is_empty());
    }

    #[test]
    fn test_generate_index() {
        let cmd = DocCommand::default();
        let file_docs = vec![
            FileDoc {
                path: PathBuf::from("a.kr"),
                module_docs: vec![],
                items: vec![DocItem {
                    name: "foo".into(),
                    kind: "function",
                    lines: vec!["A function.".into()],
                    line_number: 1,
                }],
            },
        ];
        let html = cmd.generate_index(&file_docs);
        assert!(html.contains("a.kr"));
        assert!(html.contains("1 items"));
        assert!(html.contains("filterFiles"));
    }

    #[test]
    fn test_generate_html_no_docs() {
        let cmd = DocCommand::default();
        let fd = FileDoc {
            path: PathBuf::from("empty.kr"),
            module_docs: vec![],
            items: vec![],
        };
        let html = cmd.generate_html(&fd, &[]);
        assert!(html.contains("No documentation comments found"));
    }

    #[test]
    fn test_escape_html_quotes() {
        assert_eq!(DocCommand::escape_html("\"hi\""), "&quot;hi&quot;");
    }
}
