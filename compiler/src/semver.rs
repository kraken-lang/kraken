//! Semantic Versioning Enforcement
//!
//! Tracks the public API surface and enforces semantic versioning rules:
//! - **Major** (breaking): removing/renaming public items, changing signatures, removing fields
//! - **Minor** (additive): adding public items, adding optional fields, new enum variants
//! - **Patch** (fix): bug fixes, doc changes, internal refactors with no API change
//!
//! The `ApiSurface` struct captures a snapshot of all public symbols. Comparing
//! two snapshots via `ApiSurface::diff` produces an `ApiDiff` that classifies
//! every change and determines the minimum required version bump.

use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

/// Semantic version with major.minor.patch components.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SemVer {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
}

impl SemVer {
    /// Create a new semantic version.
    pub fn new(major: u32, minor: u32, patch: u32) -> Self {
        Self {
            major,
            minor,
            patch,
        }
    }

    /// Parse a version string like "0.8.50".
    pub fn parse(s: &str) -> Option<Self> {
        let parts: Vec<&str> = s.split('.').collect();
        if parts.len() != 3 {
            return None;
        }
        Some(Self {
            major: parts[0].parse().ok()?,
            minor: parts[1].parse().ok()?,
            patch: parts[2].parse().ok()?,
        })
    }

    /// Return the next major version (e.g. 0.8.50 → 1.0.0).
    pub fn bump_major(&self) -> Self {
        Self {
            major: self.major + 1,
            minor: 0,
            patch: 0,
        }
    }

    /// Return the next minor version (e.g. 0.8.50 → 0.9.0).
    pub fn bump_minor(&self) -> Self {
        Self {
            major: self.major,
            minor: self.minor + 1,
            patch: 0,
        }
    }

    /// Return the next patch version (e.g. 0.8.50 → 0.8.51).
    pub fn bump_patch(&self) -> Self {
        Self {
            major: self.major,
            minor: self.minor,
            patch: self.patch + 1,
        }
    }
}

impl fmt::Display for SemVer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

/// The kind of public API symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SymbolKind {
    /// `pub struct Name { fields... }`
    Struct,
    /// `pub enum Name { variants... }`
    Enum,
    /// `pub trait Name { methods... }`
    Trait,
    /// `pub fn name(params) -> ret`
    Function,
    /// `pub type Name = ...`
    TypeAlias,
    /// `pub mod name`
    Module,
}

impl fmt::Display for SymbolKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SymbolKind::Struct => write!(f, "struct"),
            SymbolKind::Enum => write!(f, "enum"),
            SymbolKind::Trait => write!(f, "trait"),
            SymbolKind::Function => write!(f, "fn"),
            SymbolKind::TypeAlias => write!(f, "type"),
            SymbolKind::Module => write!(f, "mod"),
        }
    }
}

/// A public API symbol with its kind and signature hash.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ApiSymbol {
    /// Fully qualified path (e.g. `kraken::parser::ast::Program`).
    pub path: String,
    /// The kind of symbol.
    pub kind: SymbolKind,
    /// A signature fingerprint for detecting signature changes.
    /// For functions: param types + return type.
    /// For structs: field names + types.
    /// For enums: variant names.
    pub signature: String,
}

/// A snapshot of the entire public API surface.
#[derive(Debug, Clone, Default)]
pub struct ApiSurface {
    /// All public symbols keyed by their fully qualified path.
    pub symbols: BTreeMap<String, ApiSymbol>,
}

impl ApiSurface {
    /// Create a new empty API surface.
    pub fn new() -> Self {
        Self {
            symbols: BTreeMap::new(),
        }
    }

    /// Register a public symbol.
    pub fn add_symbol(&mut self, symbol: ApiSymbol) {
        self.symbols.insert(symbol.path.clone(), symbol);
    }

    /// Return the total number of public symbols.
    pub fn symbol_count(&self) -> usize {
        self.symbols.len()
    }

    /// Compute the diff between this surface (old) and another (new).
    pub fn diff(&self, new: &ApiSurface) -> ApiDiff {
        let old_paths: BTreeSet<&String> = self.symbols.keys().collect();
        let new_paths: BTreeSet<&String> = new.symbols.keys().collect();

        let mut changes = Vec::new();

        // Removed symbols (breaking)
        for path in old_paths.difference(&new_paths) {
            let sym = &self.symbols[*path];
            changes.push(ApiChange {
                path: (*path).clone(),
                kind: ChangeKind::Removed,
                severity: ChangeSeverity::Breaking,
                description: format!("{} `{}` was removed", sym.kind, path),
            });
        }

        // Added symbols (additive)
        for path in new_paths.difference(&old_paths) {
            let sym = &new.symbols[*path];
            changes.push(ApiChange {
                path: (*path).clone(),
                kind: ChangeKind::Added,
                severity: ChangeSeverity::Additive,
                description: format!("{} `{}` was added", sym.kind, path),
            });
        }

        // Changed signatures (breaking)
        for path in old_paths.intersection(&new_paths) {
            let old_sym = &self.symbols[*path];
            let new_sym = &new.symbols[*path];

            if old_sym.signature != new_sym.signature {
                changes.push(ApiChange {
                    path: (*path).clone(),
                    kind: ChangeKind::SignatureChanged,
                    severity: ChangeSeverity::Breaking,
                    description: format!(
                        "{} `{}` signature changed: `{}` → `{}`",
                        old_sym.kind, path, old_sym.signature, new_sym.signature
                    ),
                });
            }

            if old_sym.kind != new_sym.kind {
                changes.push(ApiChange {
                    path: (*path).clone(),
                    kind: ChangeKind::KindChanged,
                    severity: ChangeSeverity::Breaking,
                    description: format!(
                        "`{}` changed from {} to {}",
                        path, old_sym.kind, new_sym.kind
                    ),
                });
            }
        }

        ApiDiff { changes }
    }
}

/// The kind of API change detected.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ChangeKind {
    /// A symbol was removed from the public API.
    Removed,
    /// A symbol was added to the public API.
    Added,
    /// A symbol's signature (params, fields, variants) changed.
    SignatureChanged,
    /// A symbol's kind changed (e.g. struct → enum).
    KindChanged,
}

/// How severe the change is for semver purposes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ChangeSeverity {
    /// No API change (patch bump).
    Patch,
    /// Additive change only (minor bump).
    Additive,
    /// Breaking change (major bump).
    Breaking,
}

impl fmt::Display for ChangeSeverity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ChangeSeverity::Patch => write!(f, "patch"),
            ChangeSeverity::Additive => write!(f, "minor"),
            ChangeSeverity::Breaking => write!(f, "BREAKING"),
        }
    }
}

/// A single API change between two versions.
#[derive(Debug, Clone)]
pub struct ApiChange {
    /// The fully qualified path of the affected symbol.
    pub path: String,
    /// What kind of change occurred.
    pub kind: ChangeKind,
    /// The severity classification.
    pub severity: ChangeSeverity,
    /// Human-readable description.
    pub description: String,
}

/// The complete diff between two API surfaces.
#[derive(Debug, Clone)]
pub struct ApiDiff {
    /// All detected changes.
    pub changes: Vec<ApiChange>,
}

impl ApiDiff {
    /// Return the minimum required version bump based on the changes.
    pub fn required_bump(&self) -> ChangeSeverity {
        self.changes
            .iter()
            .map(|c| c.severity)
            .max()
            .unwrap_or(ChangeSeverity::Patch)
    }

    /// Return true if there are any breaking changes.
    pub fn has_breaking_changes(&self) -> bool {
        self.changes
            .iter()
            .any(|c| c.severity == ChangeSeverity::Breaking)
    }

    /// Return the number of changes by severity.
    pub fn count_by_severity(&self) -> (usize, usize, usize) {
        let breaking = self
            .changes
            .iter()
            .filter(|c| c.severity == ChangeSeverity::Breaking)
            .count();
        let additive = self
            .changes
            .iter()
            .filter(|c| c.severity == ChangeSeverity::Additive)
            .count();
        let patch = self
            .changes
            .iter()
            .filter(|c| c.severity == ChangeSeverity::Patch)
            .count();
        (breaking, additive, patch)
    }

    /// Validate that a proposed version bump is sufficient for the detected changes.
    pub fn validate_bump(&self, old: &SemVer, new: &SemVer) -> Result<(), String> {
        let required = self.required_bump();

        match required {
            ChangeSeverity::Breaking => {
                // Pre-1.0: breaking changes require minor bump
                // Post-1.0: breaking changes require major bump
                if old.major == 0 {
                    if new.minor <= old.minor {
                        return Err(format!(
                            "Breaking changes detected but version bump {old} → {new} is insufficient. \
                             Pre-1.0 breaking changes require at least a minor bump to {}", old.bump_minor()
                        ));
                    }
                } else if new.major <= old.major {
                    return Err(format!(
                        "Breaking changes detected but version bump {old} → {new} is insufficient. \
                         Post-1.0 breaking changes require a major bump to {}", old.bump_major()
                    ));
                }
            }
            ChangeSeverity::Additive => {
                if old.major == 0 {
                    // Pre-1.0: additive changes need at least patch bump
                    if new.patch <= old.patch && new.minor <= old.minor {
                        return Err(format!(
                            "Additive changes detected but no version bump from {old}"
                        ));
                    }
                } else if new.minor <= old.minor && new.major <= old.major {
                    return Err(format!(
                        "Additive changes detected but version bump {old} → {new} is insufficient. \
                         Additive changes require at least a minor bump to {}", old.bump_minor()
                    ));
                }
            }
            ChangeSeverity::Patch => {
                // Any bump is fine for patch-level changes
                if new <= old {
                    return Err(format!("Version {new} is not greater than {old}"));
                }
            }
        }

        Ok(())
    }
}

impl fmt::Display for ApiDiff {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (breaking, additive, _patch) = self.count_by_severity();
        writeln!(
            f,
            "API Diff: {} breaking, {} additive, {} total changes",
            breaking,
            additive,
            self.changes.len()
        )?;
        writeln!(f, "Required bump: {}", self.required_bump())?;
        writeln!(f)?;

        for change in &self.changes {
            writeln!(f, "  [{}] {}", change.severity, change.description)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_semver_parse() {
        let v = SemVer::parse("0.8.50").unwrap();
        assert_eq!(v.major, 0);
        assert_eq!(v.minor, 8);
        assert_eq!(v.patch, 50);
        assert_eq!(v.to_string(), "0.8.50");
    }

    #[test]
    fn test_semver_parse_invalid() {
        assert!(SemVer::parse("1.2").is_none());
        assert!(SemVer::parse("abc").is_none());
        assert!(SemVer::parse("1.2.3.4").is_none());
    }

    #[test]
    fn test_semver_bumps() {
        let v = SemVer::new(0, 8, 50);
        assert_eq!(v.bump_major(), SemVer::new(1, 0, 0));
        assert_eq!(v.bump_minor(), SemVer::new(0, 9, 0));
        assert_eq!(v.bump_patch(), SemVer::new(0, 8, 51));
    }

    #[test]
    fn test_empty_diff() {
        let surface = ApiSurface::new();
        let diff = surface.diff(&surface);
        assert!(diff.changes.is_empty());
        assert_eq!(diff.required_bump(), ChangeSeverity::Patch);
        assert!(!diff.has_breaking_changes());
    }

    #[test]
    fn test_added_symbol() {
        let old = ApiSurface::new();
        let mut new = ApiSurface::new();
        new.add_symbol(ApiSymbol {
            path: "kraken::parser::ast::NewNode".to_string(),
            kind: SymbolKind::Struct,
            signature: "{ field: i64 }".to_string(),
        });

        let diff = old.diff(&new);
        assert_eq!(diff.changes.len(), 1);
        assert_eq!(diff.changes[0].kind, ChangeKind::Added);
        assert_eq!(diff.changes[0].severity, ChangeSeverity::Additive);
        assert_eq!(diff.required_bump(), ChangeSeverity::Additive);
    }

    #[test]
    fn test_removed_symbol_is_breaking() {
        let mut old = ApiSurface::new();
        old.add_symbol(ApiSymbol {
            path: "kraken::error::OldError".to_string(),
            kind: SymbolKind::Enum,
            signature: "{ A, B }".to_string(),
        });
        let new = ApiSurface::new();

        let diff = old.diff(&new);
        assert_eq!(diff.changes.len(), 1);
        assert_eq!(diff.changes[0].severity, ChangeSeverity::Breaking);
        assert!(diff.has_breaking_changes());
    }

    #[test]
    fn test_signature_change_is_breaking() {
        let mut old = ApiSurface::new();
        old.add_symbol(ApiSymbol {
            path: "kraken::codegen::LLVMCodegen::generate".to_string(),
            kind: SymbolKind::Function,
            signature: "(&mut self, &Program) -> CompilerResult<()>".to_string(),
        });

        let mut new = ApiSurface::new();
        new.add_symbol(ApiSymbol {
            path: "kraken::codegen::LLVMCodegen::generate".to_string(),
            kind: SymbolKind::Function,
            signature: "(&mut self, &Program, bool) -> CompilerResult<()>".to_string(),
        });

        let diff = old.diff(&new);
        assert_eq!(diff.changes.len(), 1);
        assert_eq!(diff.changes[0].kind, ChangeKind::SignatureChanged);
        assert_eq!(diff.changes[0].severity, ChangeSeverity::Breaking);
    }

    #[test]
    fn test_validate_bump_pre_1_0_breaking() {
        let mut old = ApiSurface::new();
        old.add_symbol(ApiSymbol {
            path: "kraken::Removed".to_string(),
            kind: SymbolKind::Struct,
            signature: "{}".to_string(),
        });
        let new = ApiSurface::new();
        let diff = old.diff(&new);

        // Pre-1.0: breaking changes need minor bump
        let v_old = SemVer::new(0, 8, 50);
        let v_patch = SemVer::new(0, 8, 51);
        let v_minor = SemVer::new(0, 9, 0);

        assert!(diff.validate_bump(&v_old, &v_patch).is_err());
        assert!(diff.validate_bump(&v_old, &v_minor).is_ok());
    }

    #[test]
    fn test_validate_bump_post_1_0_breaking() {
        let mut old = ApiSurface::new();
        old.add_symbol(ApiSymbol {
            path: "kraken::Removed".to_string(),
            kind: SymbolKind::Struct,
            signature: "{}".to_string(),
        });
        let new = ApiSurface::new();
        let diff = old.diff(&new);

        // Post-1.0: breaking changes need major bump
        let v_old = SemVer::new(1, 0, 0);
        let v_minor = SemVer::new(1, 1, 0);
        let v_major = SemVer::new(2, 0, 0);

        assert!(diff.validate_bump(&v_old, &v_minor).is_err());
        assert!(diff.validate_bump(&v_old, &v_major).is_ok());
    }

    #[test]
    fn test_validate_bump_additive() {
        let old = ApiSurface::new();
        let mut new = ApiSurface::new();
        new.add_symbol(ApiSymbol {
            path: "kraken::NewThing".to_string(),
            kind: SymbolKind::Function,
            signature: "() -> ()".to_string(),
        });
        let diff = old.diff(&new);

        let v_old = SemVer::new(0, 8, 50);
        let v_bump = SemVer::new(0, 8, 51);
        assert!(diff.validate_bump(&v_old, &v_bump).is_ok());
    }

    #[test]
    fn test_mixed_changes() {
        let mut old = ApiSurface::new();
        old.add_symbol(ApiSymbol {
            path: "kraken::Kept".to_string(),
            kind: SymbolKind::Struct,
            signature: "{ a: i64 }".to_string(),
        });
        old.add_symbol(ApiSymbol {
            path: "kraken::Removed".to_string(),
            kind: SymbolKind::Function,
            signature: "() -> ()".to_string(),
        });

        let mut new = ApiSurface::new();
        new.add_symbol(ApiSymbol {
            path: "kraken::Kept".to_string(),
            kind: SymbolKind::Struct,
            signature: "{ a: i64 }".to_string(),
        });
        new.add_symbol(ApiSymbol {
            path: "kraken::Added".to_string(),
            kind: SymbolKind::Enum,
            signature: "{ X, Y }".to_string(),
        });

        let diff = old.diff(&new);
        assert_eq!(diff.changes.len(), 2); // 1 removed + 1 added
        assert!(diff.has_breaking_changes());
        let (breaking, additive, _) = diff.count_by_severity();
        assert_eq!(breaking, 1);
        assert_eq!(additive, 1);
    }

    #[test]
    fn test_kind_change_is_breaking() {
        let mut old = ApiSurface::new();
        old.add_symbol(ApiSymbol {
            path: "kraken::Thing".to_string(),
            kind: SymbolKind::Struct,
            signature: "{}".to_string(),
        });

        let mut new = ApiSurface::new();
        new.add_symbol(ApiSymbol {
            path: "kraken::Thing".to_string(),
            kind: SymbolKind::Enum,
            signature: "{}".to_string(),
        });

        let diff = old.diff(&new);
        assert_eq!(diff.changes.len(), 1);
        assert_eq!(diff.changes[0].kind, ChangeKind::KindChanged);
        assert!(diff.has_breaking_changes());
    }

    #[test]
    fn test_api_surface_count() {
        let mut surface = ApiSurface::new();
        assert_eq!(surface.symbol_count(), 0);

        surface.add_symbol(ApiSymbol {
            path: "a".to_string(),
            kind: SymbolKind::Function,
            signature: "()".to_string(),
        });
        surface.add_symbol(ApiSymbol {
            path: "b".to_string(),
            kind: SymbolKind::Struct,
            signature: "{}".to_string(),
        });
        assert_eq!(surface.symbol_count(), 2);
    }

    #[test]
    fn test_diff_display() {
        let mut old = ApiSurface::new();
        old.add_symbol(ApiSymbol {
            path: "kraken::Removed".to_string(),
            kind: SymbolKind::Struct,
            signature: "{}".to_string(),
        });
        let new = ApiSurface::new();
        let diff = old.diff(&new);
        let output = format!("{diff}");
        assert!(output.contains("BREAKING"));
        assert!(output.contains("was removed"));
    }

    #[test]
    fn test_version_ordering() {
        assert!(SemVer::new(0, 8, 50) < SemVer::new(0, 9, 0));
        assert!(SemVer::new(0, 9, 0) < SemVer::new(1, 0, 0));
        assert!(SemVer::new(1, 0, 0) > SemVer::new(0, 99, 99));
    }

    // --- SemVer edge cases ---

    #[test]
    fn test_semver_parse_non_numeric() {
        assert!(SemVer::parse("a.b.c").is_none());
        assert!(SemVer::parse("1.2.x").is_none());
        assert!(SemVer::parse("1..3").is_none());
    }

    #[test]
    fn test_semver_parse_empty() {
        assert!(SemVer::parse("").is_none());
    }

    #[test]
    fn test_semver_clone_eq() {
        let v = SemVer::new(1, 2, 3);
        let v2 = v.clone();
        assert_eq!(v, v2);
    }

    #[test]
    fn test_semver_debug() {
        let v = SemVer::new(1, 2, 3);
        let s = format!("{:?}", v);
        assert!(s.contains("SemVer"));
    }

    // --- SymbolKind Display all variants ---

    #[test]
    fn test_symbol_kind_display_all() {
        assert_eq!(format!("{}", SymbolKind::Struct), "struct");
        assert_eq!(format!("{}", SymbolKind::Enum), "enum");
        assert_eq!(format!("{}", SymbolKind::Trait), "trait");
        assert_eq!(format!("{}", SymbolKind::Function), "fn");
        assert_eq!(format!("{}", SymbolKind::TypeAlias), "type");
        assert_eq!(format!("{}", SymbolKind::Module), "mod");
    }

    // --- ChangeSeverity Display ---

    #[test]
    fn test_change_severity_display() {
        assert_eq!(format!("{}", ChangeSeverity::Patch), "patch");
        assert_eq!(format!("{}", ChangeSeverity::Additive), "minor");
        assert_eq!(format!("{}", ChangeSeverity::Breaking), "BREAKING");
    }

    #[test]
    fn test_change_severity_ordering() {
        assert!(ChangeSeverity::Patch < ChangeSeverity::Additive);
        assert!(ChangeSeverity::Additive < ChangeSeverity::Breaking);
    }

    // --- ApiSurface Default ---

    #[test]
    fn test_api_surface_default() {
        let s = ApiSurface::default();
        assert_eq!(s.symbol_count(), 0);
    }

    // --- validate_bump: additive post-1.0 insufficient ---

    #[test]
    fn test_validate_bump_additive_post_1_0_insufficient() {
        let old = ApiSurface::new();
        let mut new = ApiSurface::new();
        new.add_symbol(ApiSymbol {
            path: "kraken::New".into(),
            kind: SymbolKind::Function,
            signature: "()".into(),
        });
        let diff = old.diff(&new);

        let v_old = SemVer::new(1, 5, 0);
        let v_patch = SemVer::new(1, 5, 1);
        let v_minor = SemVer::new(1, 6, 0);

        assert!(diff.validate_bump(&v_old, &v_patch).is_err());
        assert!(diff.validate_bump(&v_old, &v_minor).is_ok());
    }

    // --- validate_bump: additive pre-1.0 no bump at all ---

    #[test]
    fn test_validate_bump_additive_pre_1_0_no_bump() {
        let old = ApiSurface::new();
        let mut new = ApiSurface::new();
        new.add_symbol(ApiSymbol {
            path: "kraken::New".into(),
            kind: SymbolKind::Function,
            signature: "()".into(),
        });
        let diff = old.diff(&new);

        let v = SemVer::new(0, 8, 50);
        assert!(diff.validate_bump(&v, &v).is_err());
    }

    // --- validate_bump: patch no-bump error ---

    #[test]
    fn test_validate_bump_patch_no_bump() {
        let surface = ApiSurface::new();
        let diff = surface.diff(&surface);
        // Empty diff = Patch severity, but version must still increase
        let v = SemVer::new(1, 0, 0);
        assert!(diff.validate_bump(&v, &v).is_err());
    }

    #[test]
    fn test_validate_bump_patch_ok() {
        let surface = ApiSurface::new();
        let diff = surface.diff(&surface);
        let v_old = SemVer::new(1, 0, 0);
        let v_new = SemVer::new(1, 0, 1);
        assert!(diff.validate_bump(&v_old, &v_new).is_ok());
    }

    // --- ApiSymbol overwrite ---

    #[test]
    fn test_add_symbol_overwrites() {
        let mut s = ApiSurface::new();
        s.add_symbol(ApiSymbol {
            path: "a".into(),
            kind: SymbolKind::Function,
            signature: "v1".into(),
        });
        s.add_symbol(ApiSymbol {
            path: "a".into(),
            kind: SymbolKind::Function,
            signature: "v2".into(),
        });
        assert_eq!(s.symbol_count(), 1);
        assert_eq!(s.symbols["a"].signature, "v2");
    }

    // --- ApiDiff Display with additive + breaking ---

    #[test]
    fn test_diff_display_mixed() {
        let mut old = ApiSurface::new();
        old.add_symbol(ApiSymbol {
            path: "r".into(),
            kind: SymbolKind::Struct,
            signature: "{}".into(),
        });
        let mut new = ApiSurface::new();
        new.add_symbol(ApiSymbol {
            path: "a".into(),
            kind: SymbolKind::Enum,
            signature: "{}".into(),
        });
        let diff = old.diff(&new);
        let output = format!("{diff}");
        assert!(output.contains("BREAKING"));
        assert!(output.contains("minor"));
        assert!(output.contains("2 total"));
    }

    // --- ChangeKind/ApiChange clone/debug ---

    #[test]
    fn test_change_kind_clone_eq() {
        let k = ChangeKind::Removed;
        assert_eq!(k.clone(), ChangeKind::Removed);
        assert_ne!(k, ChangeKind::Added);
    }

    #[test]
    fn test_api_change_debug() {
        let c = ApiChange {
            path: "x".into(),
            kind: ChangeKind::Added,
            severity: ChangeSeverity::Additive,
            description: "added x".into(),
        };
        let s = format!("{:?}", c);
        assert!(s.contains("Added"));
    }

    // --- ApiDiff count_by_severity with patch ---

    #[test]
    fn test_count_by_severity_empty() {
        let diff = ApiDiff { changes: vec![] };
        assert_eq!(diff.count_by_severity(), (0, 0, 0));
        assert!(!diff.has_breaking_changes());
    }

    // --- Signature AND kind change on same symbol ---

    #[test]
    fn test_signature_and_kind_change() {
        let mut old = ApiSurface::new();
        old.add_symbol(ApiSymbol {
            path: "x".into(),
            kind: SymbolKind::Struct,
            signature: "{ a: i64 }".into(),
        });
        let mut new = ApiSurface::new();
        new.add_symbol(ApiSymbol {
            path: "x".into(),
            kind: SymbolKind::Enum,
            signature: "{ A, B }".into(),
        });
        let diff = old.diff(&new);
        assert_eq!(diff.changes.len(), 2); // signature + kind
        assert!(diff.has_breaking_changes());
    }
}
