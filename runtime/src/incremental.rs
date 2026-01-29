//! Incremental compilation support for bootstrap compiler.
//!
//! Provides infrastructure for tracking compilation dependencies and
//! enabling incremental builds.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::time::SystemTime;

/// Compilation unit metadata.
#[derive(Debug, Clone)]
pub struct CompilationUnit {
    pub path: PathBuf,
    pub last_modified: SystemTime,
    pub dependencies: HashSet<PathBuf>,
    pub hash: u64,
}

impl CompilationUnit {
    /// Create a new compilation unit.
    pub fn new(path: PathBuf) -> Self {
        Self {
            path,
            last_modified: SystemTime::now(),
            dependencies: HashSet::new(),
            hash: 0,
        }
    }

    /// Add a dependency.
    pub fn add_dependency(&mut self, dep: PathBuf) {
        self.dependencies.insert(dep);
    }

    /// Check if this unit needs recompilation.
    pub fn needs_recompilation(&self, cache: &CompilationCache) -> bool {
        // Check if file was modified
        if let Ok(metadata) = std::fs::metadata(&self.path) {
            if let Ok(modified) = metadata.modified() {
                if modified > self.last_modified {
                    return true;
                }
            }
        }

        // Check if any dependency was modified
        for dep in &self.dependencies {
            if let Some(dep_unit) = cache.get(dep) {
                if dep_unit.last_modified > self.last_modified {
                    return true;
                }
            }
        }

        false
    }
}

/// Compilation cache for incremental builds.
#[derive(Debug, Clone)]
pub struct CompilationCache {
    units: HashMap<PathBuf, CompilationUnit>,
    dirty: HashSet<PathBuf>,
}

impl CompilationCache {
    /// Create a new compilation cache.
    pub fn new() -> Self {
        Self {
            units: HashMap::new(),
            dirty: HashSet::new(),
        }
    }

    /// Add a compilation unit to the cache.
    pub fn insert(&mut self, unit: CompilationUnit) {
        self.units.insert(unit.path.clone(), unit);
    }

    /// Get a compilation unit from the cache.
    pub fn get(&self, path: &Path) -> Option<&CompilationUnit> {
        self.units.get(path)
    }

    /// Mark a unit as dirty (needs recompilation).
    pub fn mark_dirty(&mut self, path: PathBuf) {
        self.dirty.insert(path);
    }

    /// Check if a unit is dirty.
    pub fn is_dirty(&self, path: &Path) -> bool {
        self.dirty.contains(path)
    }

    /// Get all dirty units.
    pub fn dirty_units(&self) -> Vec<&CompilationUnit> {
        self.dirty
            .iter()
            .filter_map(|path| self.units.get(path))
            .collect()
    }

    /// Clear dirty flags.
    pub fn clear_dirty(&mut self) {
        self.dirty.clear();
    }

    /// Compute transitive dependencies.
    pub fn transitive_dependencies(&self, path: &Path) -> HashSet<PathBuf> {
        let mut result = HashSet::new();
        let mut to_visit = vec![path.to_path_buf()];

        while let Some(current) = to_visit.pop() {
            if result.contains(&current) {
                continue;
            }
            result.insert(current.clone());

            if let Some(unit) = self.units.get(&current) {
                for dep in &unit.dependencies {
                    if !result.contains(dep) {
                        to_visit.push(dep.clone());
                    }
                }
            }
        }

        result
    }

    /// Get compilation order (topological sort).
    pub fn compilation_order(&self) -> Vec<PathBuf> {
        let mut result = Vec::new();
        let mut visited = HashSet::new();
        let mut temp_mark = HashSet::new();

        for path in self.units.keys() {
            if !visited.contains(path) {
                self.visit_for_topo_sort(path, &mut visited, &mut temp_mark, &mut result);
            }
        }

        result
    }

    fn visit_for_topo_sort(
        &self,
        path: &PathBuf,
        visited: &mut HashSet<PathBuf>,
        temp_mark: &mut HashSet<PathBuf>,
        result: &mut Vec<PathBuf>,
    ) {
        if visited.contains(path) {
            return;
        }

        if temp_mark.contains(path) {
            // Cycle detected, skip
            return;
        }

        temp_mark.insert(path.clone());

        if let Some(unit) = self.units.get(path) {
            for dep in &unit.dependencies {
                self.visit_for_topo_sort(dep, visited, temp_mark, result);
            }
        }

        temp_mark.remove(path);
        visited.insert(path.clone());
        result.push(path.clone());
    }
}

impl Default for CompilationCache {
    fn default() -> Self {
        Self::new()
    }
}

/// Dependency graph for tracking compilation dependencies.
#[derive(Debug, Clone)]
pub struct DependencyGraph {
    edges: HashMap<PathBuf, HashSet<PathBuf>>,
}

impl DependencyGraph {
    /// Create a new dependency graph.
    pub fn new() -> Self {
        Self {
            edges: HashMap::new(),
        }
    }

    /// Add an edge from source to target.
    pub fn add_edge(&mut self, source: PathBuf, target: PathBuf) {
        self.edges.entry(source).or_default().insert(target);
    }

    /// Get dependencies of a node.
    pub fn dependencies(&self, node: &Path) -> Option<&HashSet<PathBuf>> {
        self.edges.get(node)
    }

    /// Check if there's a path from source to target.
    pub fn has_path(&self, source: &Path, target: &Path) -> bool {
        let mut visited = HashSet::new();
        let mut to_visit = vec![source.to_path_buf()];

        while let Some(current) = to_visit.pop() {
            if current == target {
                return true;
            }

            if visited.contains(&current) {
                continue;
            }
            visited.insert(current.clone());

            if let Some(deps) = self.edges.get(&current) {
                for dep in deps {
                    if !visited.contains(dep) {
                        to_visit.push(dep.clone());
                    }
                }
            }
        }

        false
    }

    /// Detect cycles in the graph.
    pub fn has_cycle(&self) -> bool {
        let mut visited = HashSet::new();
        let mut rec_stack = HashSet::new();

        for node in self.edges.keys() {
            if !visited.contains(node) && self.has_cycle_util(node, &mut visited, &mut rec_stack) {
                return true;
            }
        }

        false
    }

    fn has_cycle_util(
        &self,
        node: &PathBuf,
        visited: &mut HashSet<PathBuf>,
        rec_stack: &mut HashSet<PathBuf>,
    ) -> bool {
        visited.insert(node.clone());
        rec_stack.insert(node.clone());

        if let Some(deps) = self.edges.get(node) {
            for dep in deps {
                if !visited.contains(dep) {
                    if self.has_cycle_util(dep, visited, rec_stack) {
                        return true;
                    }
                } else if rec_stack.contains(dep) {
                    return true;
                }
            }
        }

        rec_stack.remove(node);
        false
    }
}

impl Default for DependencyGraph {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compilation_unit() {
        let mut unit = CompilationUnit::new(PathBuf::from("test.kr"));
        assert_eq!(unit.dependencies.len(), 0);

        unit.add_dependency(PathBuf::from("dep.kr"));
        assert_eq!(unit.dependencies.len(), 1);
    }

    #[test]
    fn test_compilation_cache() {
        let mut cache = CompilationCache::new();
        let unit = CompilationUnit::new(PathBuf::from("test.kr"));

        cache.insert(unit.clone());
        assert!(cache.get(Path::new("test.kr")).is_some());
    }

    #[test]
    fn test_cache_dirty_tracking() {
        let mut cache = CompilationCache::new();
        let path = PathBuf::from("test.kr");

        cache.mark_dirty(path.clone());
        assert!(cache.is_dirty(&path));

        cache.clear_dirty();
        assert!(!cache.is_dirty(&path));
    }

    #[test]
    fn test_transitive_dependencies() {
        let mut cache = CompilationCache::new();

        let mut unit1 = CompilationUnit::new(PathBuf::from("a.kr"));
        unit1.add_dependency(PathBuf::from("b.kr"));
        cache.insert(unit1);

        let mut unit2 = CompilationUnit::new(PathBuf::from("b.kr"));
        unit2.add_dependency(PathBuf::from("c.kr"));
        cache.insert(unit2);

        let deps = cache.transitive_dependencies(Path::new("a.kr"));
        assert!(deps.contains(&PathBuf::from("a.kr")));
        assert!(deps.contains(&PathBuf::from("b.kr")));
        assert!(deps.contains(&PathBuf::from("c.kr")));
    }

    #[test]
    fn test_compilation_order() {
        let mut cache = CompilationCache::new();

        let mut unit1 = CompilationUnit::new(PathBuf::from("a.kr"));
        unit1.add_dependency(PathBuf::from("b.kr"));
        cache.insert(unit1);

        let unit2 = CompilationUnit::new(PathBuf::from("b.kr"));
        cache.insert(unit2);

        let order = cache.compilation_order();
        assert_eq!(order.len(), 2);
    }

    #[test]
    fn test_dependency_graph() {
        let mut graph = DependencyGraph::new();
        graph.add_edge(PathBuf::from("a.kr"), PathBuf::from("b.kr"));
        graph.add_edge(PathBuf::from("b.kr"), PathBuf::from("c.kr"));

        assert!(graph.has_path(Path::new("a.kr"), Path::new("c.kr")));
        assert!(!graph.has_path(Path::new("c.kr"), Path::new("a.kr")));
    }

    #[test]
    fn test_cycle_detection() {
        let mut graph = DependencyGraph::new();
        graph.add_edge(PathBuf::from("a.kr"), PathBuf::from("b.kr"));
        graph.add_edge(PathBuf::from("b.kr"), PathBuf::from("c.kr"));
        assert!(!graph.has_cycle());

        graph.add_edge(PathBuf::from("c.kr"), PathBuf::from("a.kr"));
        assert!(graph.has_cycle());
    }
}
