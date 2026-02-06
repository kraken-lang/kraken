//! Doc command - Generate documentation from source code.

use crate::cli::output::{OutputMessage, ProgressIndicator};
use crate::cli::{Command, CommandResult};
use glob::glob;
use std::fs;
use std::path::{Path, PathBuf};

/// Doc command: generates HTML documentation from `///` and `//!` doc comments.
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

    fn extract_doc_comments(&self, source: &str) -> Vec<String> {
        let mut docs = Vec::new();
        for line in source.lines() {
            let trimmed = line.trim();
            if let Some(stripped) = trimmed
                .strip_prefix("///")
                .or_else(|| trimmed.strip_prefix("//!"))
            {
                docs.push(stripped.trim().to_string());
            }
        }
        docs
    }

    fn generate_html(&self, file_path: &Path, docs: &[String]) -> String {
        let file_name = file_path.file_name().unwrap().to_string_lossy();
        let mut html = format!(
            r#"<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>{file_name} - Kraken Documentation</title>
    <style>
        body {{ font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; 
               max-width: 900px; margin: 0 auto; padding: 20px; }}
        h1 {{ color: #2c3e50; }}
        .doc-comment {{ background: #f8f9fa; padding: 15px; margin: 10px 0; 
                       border-left: 4px solid #3498db; }}
        code {{ background: #e9ecef; padding: 2px 6px; border-radius: 3px; }}
        pre {{ background: #2c3e50; color: #ecf0f1; padding: 15px; 
              border-radius: 5px; overflow-x: auto; }}
    </style>
</head>
<body>
    <h1>Documentation for {file_name}</h1>
"#
        );

        if docs.is_empty() {
            html.push_str("<p><em>No documentation comments found.</em></p>");
        } else {
            for doc in docs {
                html.push_str(&format!("<div class='doc-comment'>{doc}</div>\n"));
            }
        }

        html.push_str(
            r#"
</body>
</html>
"#,
        );

        html
    }

    fn generate_index(&self, files: &[PathBuf]) -> String {
        let mut html = r#"<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Kraken Documentation Index</title>
    <style>
        body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; 
               max-width: 900px; margin: 0 auto; padding: 20px; }
        h1 { color: #2c3e50; }
        ul { list-style: none; padding: 0; }
        li { padding: 10px; margin: 5px 0; background: #f8f9fa; border-radius: 5px; }
        a { color: #3498db; text-decoration: none; }
        a:hover { text-decoration: underline; }
    </style>
</head>
<body>
    <h1>Kraken Documentation</h1>
    <h2>Source Files</h2>
    <ul>
"#
        .to_string();

        for file in files {
            let file_name = file.file_name().unwrap().to_string_lossy();
            let html_name = format!("{file_name}.html");
            html.push_str(&format!(
                "        <li><a href='{html_name}'>{file_name}</a></li>\n"
            ));
        }

        html.push_str(
            r#"    </ul>
</body>
</html>
"#,
        );

        html
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

        let progress = ProgressIndicator::new("Generating docs", files.len() as u64);

        for (i, file) in files.iter().enumerate() {
            progress.update((i + 1) as u64);

            let source = fs::read_to_string(file)
                .map_err(|e| format!("Failed to read {}: {e}", file.display()))?;

            let docs = self.extract_doc_comments(&source);
            let html = self.generate_html(file, &docs);

            let file_name = file.file_name().unwrap().to_string_lossy();
            let output_path = self.output_dir.join(format!("{file_name}.html"));

            fs::write(&output_path, html)
                .map_err(|e| format!("Failed to write {}: {e}", output_path.display()))?;
        }

        // Generate index
        let index_html = self.generate_index(&files);
        fs::write(self.output_dir.join("index.html"), index_html)
            .map_err(|e| format!("Failed to write index.html: {e}"))?;

        progress.finish("Documentation generated");

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
    fn test_extract_doc_comments() {
        let cmd = DocCommand::default();
        let source = r#"
/// This is a doc comment
/// Second line
fn test() {}
//! Module doc
"#;
        let docs = cmd.extract_doc_comments(source);
        assert_eq!(docs.len(), 3);
        assert_eq!(docs[0], "This is a doc comment");
    }
}
