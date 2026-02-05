//! Simple XML parser and writer.
//!
//! Provides basic XML parsing and generation capabilities.

#![allow(dead_code)]

use std::collections::HashMap;

/// XML node representing an element
#[derive(Debug, Clone, PartialEq)]
pub struct XmlNode {
    /// Tag name
    pub tag: String,
    /// Attributes
    pub attributes: HashMap<String, String>,
    /// Child nodes
    pub children: Vec<XmlNode>,
    /// Text content
    pub text: Option<String>,
}

impl XmlNode {
    /// Create a new XML node
    pub fn new(tag: impl Into<String>) -> Self {
        Self {
            tag: tag.into(),
            attributes: HashMap::new(),
            children: Vec::new(),
            text: None,
        }
    }

    /// Add an attribute
    pub fn with_attribute(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.attributes.insert(key.into(), value.into());
        self
    }

    /// Add a child node
    pub fn with_child(mut self, child: XmlNode) -> Self {
        self.children.push(child);
        self
    }

    /// Set text content
    pub fn with_text(mut self, text: impl Into<String>) -> Self {
        self.text = Some(text.into());
        self
    }

    /// Get attribute value
    pub fn get_attribute(&self, key: &str) -> Option<&String> {
        self.attributes.get(key)
    }

    /// Find first child with given tag name
    pub fn find_child(&self, tag: &str) -> Option<&XmlNode> {
        self.children.iter().find(|child| child.tag == tag)
    }

    /// Find all children with given tag name
    pub fn find_children(&self, tag: &str) -> Vec<&XmlNode> {
        self.children
            .iter()
            .filter(|child| child.tag == tag)
            .collect()
    }
}

/// Simple XML parser
pub struct XmlParser;

impl XmlParser {
    /// Parse XML from string
    pub fn parse(xml: &str) -> Result<XmlNode, String> {
        let xml = xml.trim();
        if xml.is_empty() {
            return Err("Empty XML string".to_string());
        }

        Self::parse_node(xml)
    }

    fn parse_node(xml: &str) -> Result<XmlNode, String> {
        let xml = xml.trim();
        
        // Find opening tag
        if !xml.starts_with('<') {
            return Err("XML must start with '<'".to_string());
        }

        let tag_end = xml.find('>').ok_or("Missing closing '>' for tag")?;
        let tag_content = &xml[1..tag_end];

        // Check for self-closing tag
        if let Some(tag_content) = tag_content.strip_suffix('/') {
            let (tag, attributes) = Self::parse_tag_and_attributes(tag_content)?;
            return Ok(XmlNode {
                tag,
                attributes,
                children: Vec::new(),
                text: None,
            });
        }

        let (tag, attributes) = Self::parse_tag_and_attributes(tag_content)?;

        // Find closing tag
        let closing_tag = format!("</{tag}>");
        let content_start = tag_end + 1;
        let content_end = xml.rfind(&closing_tag)
            .ok_or_else(|| format!("Missing closing tag for '{tag}'"))?;

        let content = &xml[content_start..content_end].trim();

        // Parse children or text content
        let (children, text) = if content.starts_with('<') {
            (Self::parse_children(content)?, None)
        } else if !content.is_empty() {
            (Vec::new(), Some(content.to_string()))
        } else {
            (Vec::new(), None)
        };

        Ok(XmlNode {
            tag,
            attributes,
            children,
            text,
        })
    }

    fn parse_tag_and_attributes(tag_content: &str) -> Result<(String, HashMap<String, String>), String> {
        let parts: Vec<&str> = tag_content.split_whitespace().collect();
        if parts.is_empty() {
            return Err("Empty tag".to_string());
        }

        let tag = parts[0].to_string();
        let mut attributes = HashMap::new();

        for part in &parts[1..] {
            if let Some(eq_pos) = part.find('=') {
                let key = part[..eq_pos].to_string();
                let value = part[eq_pos + 1..]
                    .trim_matches('"')
                    .trim_matches('\'')
                    .to_string();
                attributes.insert(key, value);
            }
        }

        Ok((tag, attributes))
    }

    fn parse_children(content: &str) -> Result<Vec<XmlNode>, String> {
        let mut children = Vec::new();
        let mut remaining = content.trim();

        while !remaining.is_empty() {
            if !remaining.starts_with('<') {
                break;
            }

            // Find the matching closing tag
            let tag_start = remaining.find('<').unwrap();
            let tag_end = remaining.find('>').ok_or("Missing closing '>'")?;
            let tag_content = &remaining[tag_start + 1..tag_end];

            let tag_name = tag_content.split_whitespace().next()
                .ok_or("Empty tag")?;

            let closing_tag = format!("</{tag_name}>");
            
            let node_end = if tag_content.ends_with('/') {
                tag_end + 1
            } else {
                remaining.find(&closing_tag)
                    .ok_or_else(|| format!("Missing closing tag for '{tag_name}'"))?
                    + closing_tag.len()
            };

            let node_xml = &remaining[..node_end];
            children.push(Self::parse_node(node_xml)?);
            remaining = remaining[node_end..].trim();
        }

        Ok(children)
    }
}

/// XML writer
pub struct XmlWriter {
    indent: bool,
    indent_size: usize,
}

impl XmlWriter {
    /// Create a new XML writer
    pub fn new() -> Self {
        Self {
            indent: true,
            indent_size: 2,
        }
    }

    /// Create XML writer without indentation
    pub fn compact() -> Self {
        Self {
            indent: false,
            indent_size: 0,
        }
    }

    /// Write XML node to string
    pub fn write(&self, node: &XmlNode) -> String {
        self.write_node(node, 0)
    }

    fn write_node(&self, node: &XmlNode, depth: usize) -> String {
        let indent = if self.indent {
            " ".repeat(depth * self.indent_size)
        } else {
            String::new()
        };

        let newline = if self.indent { "\n" } else { "" };

        let mut result = format!("{}<{}", indent, node.tag);

        // Add attributes
        for (key, value) in &node.attributes {
            result.push_str(&format!(r#" {key}="{value}""#));
        }

        // Self-closing tag if no children or text
        if node.children.is_empty() && node.text.is_none() {
            result.push_str(" />");
            result.push_str(newline);
            return result;
        }

        result.push('>');

        // Add text content
        if let Some(text) = &node.text {
            result.push_str(text);
        }

        // Add children
        if !node.children.is_empty() {
            result.push_str(newline);
            for child in &node.children {
                result.push_str(&self.write_node(child, depth + 1));
            }
            result.push_str(&indent);
        }

        result.push_str(&format!("</{}>", node.tag));
        result.push_str(newline);

        result
    }
}

impl Default for XmlWriter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_node() {
        let node = XmlNode::new("root");
        assert_eq!(node.tag, "root");
        assert!(node.attributes.is_empty());
        assert!(node.children.is_empty());
        assert!(node.text.is_none());
    }

    #[test]
    fn test_node_with_attributes() {
        let node = XmlNode::new("root")
            .with_attribute("id", "123")
            .with_attribute("name", "test");
        
        assert_eq!(node.get_attribute("id"), Some(&"123".to_string()));
        assert_eq!(node.get_attribute("name"), Some(&"test".to_string()));
    }

    #[test]
    fn test_node_with_text() {
        let node = XmlNode::new("root").with_text("Hello, World!");
        assert_eq!(node.text, Some("Hello, World!".to_string()));
    }

    #[test]
    fn test_node_with_children() {
        let child = XmlNode::new("child");
        let node = XmlNode::new("root").with_child(child);
        assert_eq!(node.children.len(), 1);
        assert_eq!(node.children[0].tag, "child");
    }

    #[test]
    fn test_parse_simple_xml() {
        let xml = "<root>Hello</root>";
        let node = XmlParser::parse(xml).unwrap();
        
        assert_eq!(node.tag, "root");
        assert_eq!(node.text, Some("Hello".to_string()));
    }

    #[test]
    fn test_parse_self_closing() {
        let xml = "<root />";
        let node = XmlParser::parse(xml).unwrap();
        
        assert_eq!(node.tag, "root");
        assert!(node.text.is_none());
        assert!(node.children.is_empty());
    }

    #[test]
    fn test_parse_with_attributes() {
        let xml = r#"<root id="123" name="test">Content</root>"#;
        let node = XmlParser::parse(xml).unwrap();
        
        assert_eq!(node.tag, "root");
        assert_eq!(node.get_attribute("id"), Some(&"123".to_string()));
        assert_eq!(node.get_attribute("name"), Some(&"test".to_string()));
    }

    #[test]
    fn test_parse_with_children() {
        let xml = "<root><child1>Text1</child1><child2>Text2</child2></root>";
        let node = XmlParser::parse(xml).unwrap();
        
        assert_eq!(node.children.len(), 2);
        assert_eq!(node.children[0].tag, "child1");
        assert_eq!(node.children[1].tag, "child2");
    }

    #[test]
    fn test_find_child() {
        let xml = "<root><child1>Text1</child1><child2>Text2</child2></root>";
        let node = XmlParser::parse(xml).unwrap();
        
        let child = node.find_child("child1");
        assert!(child.is_some());
        assert_eq!(child.unwrap().tag, "child1");
    }

    #[test]
    fn test_write_simple_xml() {
        let node = XmlNode::new("root").with_text("Hello");
        let writer = XmlWriter::new();
        let xml = writer.write(&node);
        
        assert!(xml.contains("<root>"));
        assert!(xml.contains("Hello"));
        assert!(xml.contains("</root>"));
    }

    #[test]
    fn test_write_with_attributes() {
        let node = XmlNode::new("root")
            .with_attribute("id", "123")
            .with_text("Content");
        let writer = XmlWriter::new();
        let xml = writer.write(&node);
        
        assert!(xml.contains(r#"id="123""#));
    }

    #[test]
    fn test_write_self_closing() {
        let node = XmlNode::new("root");
        let writer = XmlWriter::new();
        let xml = writer.write(&node);
        
        assert!(xml.contains("<root />"));
    }

    #[test]
    fn test_write_compact() {
        let node = XmlNode::new("root").with_text("Hello");
        let writer = XmlWriter::compact();
        let xml = writer.write(&node);
        
        assert!(!xml.contains('\n'));
    }

    #[test]
    fn test_roundtrip() {
        let original = XmlNode::new("root")
            .with_attribute("id", "123")
            .with_child(XmlNode::new("child").with_text("Text"));
        
        let writer = XmlWriter::compact();
        let xml = writer.write(&original);
        let parsed = XmlParser::parse(&xml).unwrap();
        
        assert_eq!(parsed.tag, original.tag);
        assert_eq!(parsed.get_attribute("id"), original.get_attribute("id"));
    }
}
