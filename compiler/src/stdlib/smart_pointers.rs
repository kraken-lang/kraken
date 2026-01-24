//! Smart pointer types for heap allocation and reference counting.

#![allow(dead_code)]

use crate::parser::ast::Type;

/// Smart pointer type definitions
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SmartPointerType {
    /// Box<T> - heap allocation with ownership
    Box(Box<Type>),
    /// Rc<T> - reference counting for shared ownership
    Rc(Box<Type>),
    /// Arc<T> - atomic reference counting for thread safety
    Arc(Box<Type>),
    /// Weak<T> - weak references to break cycles
    Weak(Box<Type>),
}

impl SmartPointerType {
    /// Get the inner type of the smart pointer
    pub fn inner_type(&self) -> &Type {
        match self {
            SmartPointerType::Box(t) => t,
            SmartPointerType::Rc(t) => t,
            SmartPointerType::Arc(t) => t,
            SmartPointerType::Weak(t) => t,
        }
    }

    /// Check if the smart pointer is thread-safe
    pub fn is_thread_safe(&self) -> bool {
        matches!(self, SmartPointerType::Arc(_))
    }

    /// Check if the smart pointer uses reference counting
    pub fn uses_reference_counting(&self) -> bool {
        matches!(
            self,
            SmartPointerType::Rc(_) | SmartPointerType::Arc(_) | SmartPointerType::Weak(_)
        )
    }

    /// Get the smart pointer type name
    pub fn type_name(&self) -> &str {
        match self {
            SmartPointerType::Box(_) => "Box",
            SmartPointerType::Rc(_) => "Rc",
            SmartPointerType::Arc(_) => "Arc",
            SmartPointerType::Weak(_) => "Weak",
        }
    }
}

/// Smart pointer operations
pub struct SmartPointerOps;

impl SmartPointerOps {
    /// Create a new Box<T>
    pub fn box_new() -> &'static str {
        "kraken_box_new"
    }

    /// Drop a Box<T>
    pub fn box_drop() -> &'static str {
        "kraken_box_drop"
    }

    /// Dereference a Box<T>
    pub fn box_deref() -> &'static str {
        "kraken_box_deref"
    }

    /// Create a new Rc<T>
    pub fn rc_new() -> &'static str {
        "kraken_rc_new"
    }

    /// Clone an Rc<T> (increment reference count)
    pub fn rc_clone() -> &'static str {
        "kraken_rc_clone"
    }

    /// Drop an Rc<T> (decrement reference count)
    pub fn rc_drop() -> &'static str {
        "kraken_rc_drop"
    }

    /// Get reference count of Rc<T>
    pub fn rc_strong_count() -> &'static str {
        "kraken_rc_strong_count"
    }

    /// Create a new Arc<T>
    pub fn arc_new() -> &'static str {
        "kraken_arc_new"
    }

    /// Clone an Arc<T> (atomic increment)
    pub fn arc_clone() -> &'static str {
        "kraken_arc_clone"
    }

    /// Drop an Arc<T> (atomic decrement)
    pub fn arc_drop() -> &'static str {
        "kraken_arc_drop"
    }

    /// Get reference count of Arc<T>
    pub fn arc_strong_count() -> &'static str {
        "kraken_arc_strong_count"
    }

    /// Create a weak reference from Rc<T>
    pub fn rc_downgrade() -> &'static str {
        "kraken_rc_downgrade"
    }

    /// Upgrade a weak reference to Rc<T>
    pub fn weak_upgrade() -> &'static str {
        "kraken_weak_upgrade"
    }

    /// Create a weak reference from Arc<T>
    pub fn arc_downgrade() -> &'static str {
        "kraken_arc_downgrade"
    }

    /// Upgrade a weak reference to Arc<T>
    pub fn weak_arc_upgrade() -> &'static str {
        "kraken_weak_arc_upgrade"
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_smart_pointer_type_creation() {
        let box_type = SmartPointerType::Box(Box::new(Type::Int));
        assert_eq!(box_type.type_name(), "Box");
        assert!(!box_type.is_thread_safe());
        assert!(!box_type.uses_reference_counting());
    }

    #[test]
    fn test_rc_type() {
        let rc_type = SmartPointerType::Rc(Box::new(Type::String));
        assert_eq!(rc_type.type_name(), "Rc");
        assert!(!rc_type.is_thread_safe());
        assert!(rc_type.uses_reference_counting());
    }

    #[test]
    fn test_arc_type() {
        let arc_type = SmartPointerType::Arc(Box::new(Type::Int));
        assert_eq!(arc_type.type_name(), "Arc");
        assert!(arc_type.is_thread_safe());
        assert!(arc_type.uses_reference_counting());
    }

    #[test]
    fn test_weak_type() {
        let weak_type = SmartPointerType::Weak(Box::new(Type::Bool));
        assert_eq!(weak_type.type_name(), "Weak");
        assert!(!weak_type.is_thread_safe());
        assert!(weak_type.uses_reference_counting());
    }

    #[test]
    fn test_smart_pointer_ops() {
        assert_eq!(SmartPointerOps::box_new(), "kraken_box_new");
        assert_eq!(SmartPointerOps::rc_clone(), "kraken_rc_clone");
        assert_eq!(SmartPointerOps::arc_new(), "kraken_arc_new");
        assert_eq!(SmartPointerOps::weak_upgrade(), "kraken_weak_upgrade");
    }
}
