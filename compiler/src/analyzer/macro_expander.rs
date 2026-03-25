//! Macro expansion engine for declarative macros.

use crate::error::{CompilerError, CompilerResult};
use crate::parser::ast::{Expression, MacroRule, MacroToken, Statement};
use std::collections::HashMap;

/// Macro expander for declarative macros
pub struct MacroExpander {
    macros: HashMap<String, Vec<MacroRule>>,
    hygiene_counter: usize,
}

impl Default for MacroExpander {
    fn default() -> Self {
        Self::new()
    }
}

impl MacroExpander {
    /// Create a new macro expander with no registered macros.
    pub fn new() -> Self {
        Self {
            macros: HashMap::new(),
            hygiene_counter: 0,
        }
    }

    /// Register a macro for expansion
    pub fn register_macro(&mut self, name: String, rules: Vec<MacroRule>) {
        self.macros.insert(name, rules);
    }

    /// Expand a macro invocation
    pub fn expand_macro(&mut self, name: &str, args: &[Expression]) -> CompilerResult<Statement> {
        let rules =
            self.macros.get(name).cloned().ok_or_else(|| {
                CompilerError::internal_error(format!("Macro '{name}' not found"))
            })?;

        // Try each rule in order
        for rule in &rules {
            if let Ok(expansion) = self.try_expand_rule(rule, args) {
                return Ok(expansion);
            }
        }

        Err(CompilerError::internal_error(format!(
            "No matching macro rule for '{name}'"
        )))
    }

    fn try_expand_rule(
        &mut self,
        rule: &MacroRule,
        args: &[Expression],
    ) -> CompilerResult<Statement> {
        // Match pattern against arguments
        let bindings = self.match_pattern(&rule.pattern, args)?;

        // Expand the rule body with bindings
        self.expand_tokens(&rule.expansion, &bindings)
    }

    fn match_pattern(
        &self,
        pattern: &[MacroToken],
        args: &[Expression],
    ) -> CompilerResult<HashMap<String, Vec<Expression>>> {
        let mut bindings = HashMap::new();
        let mut arg_idx = 0;

        for token in pattern {
            match token {
                MacroToken::Variable(var_name) => {
                    if arg_idx >= args.len() {
                        return Err(CompilerError::internal_error(
                            "Not enough arguments for macro pattern".to_string(),
                        ));
                    }
                    bindings.insert(var_name.clone(), vec![args[arg_idx].clone()]);
                    arg_idx += 1;
                }
                MacroToken::Repetition(rep_pattern) => {
                    let mut rep_args = Vec::new();
                    while arg_idx < args.len() {
                        rep_args.push(args[arg_idx].clone());
                        arg_idx += 1;
                    }
                    // Store repetition bindings
                    if let Some(MacroToken::Variable(var_name)) = rep_pattern.first() {
                        bindings.insert(var_name.clone(), rep_args);
                    }
                }
                MacroToken::Literal(_) => {
                    // Literal tokens are matched exactly (not implemented yet)
                    arg_idx += 1;
                }
            }
        }

        Ok(bindings)
    }

    fn expand_tokens(
        &mut self,
        tokens: &[MacroToken],
        bindings: &HashMap<String, Vec<Expression>>,
    ) -> CompilerResult<Statement> {
        // For now, create a simple expression statement
        // This is a simplified implementation - full macro expansion would be more complex
        if let Some(MacroToken::Variable(var_name)) = tokens.first() {
            if let Some(exprs) = bindings.get(var_name) {
                if let Some(expr) = exprs.first() {
                    return Ok(Statement::Expression(expr.clone()));
                }
            }
        }

        // Default: return empty expression
        Ok(Statement::Expression(Expression::IntLiteral(0)))
    }

    /// Generate a unique hygienic identifier
    pub fn generate_hygienic_name(&mut self, base: &str) -> String {
        self.hygiene_counter += 1;
        format!("{}__hygiene_{}", base, self.hygiene_counter)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn var(name: &str) -> MacroToken {
        MacroToken::Variable(name.to_string())
    }
    fn lit(s: &str) -> MacroToken {
        MacroToken::Literal(s.to_string())
    }
    fn rule(pattern: Vec<MacroToken>, expansion: Vec<MacroToken>) -> MacroRule {
        MacroRule { pattern, expansion }
    }

    #[test]
    fn test_default() {
        let e = MacroExpander::default();
        assert!(e.macros.is_empty());
        assert_eq!(e.hygiene_counter, 0);
    }

    #[test]
    fn test_new() {
        let e = MacroExpander::new();
        assert!(e.macros.is_empty());
    }

    #[test]
    fn test_register_macro() {
        let mut e = MacroExpander::new();
        e.register_macro("m".into(), vec![rule(vec![var("x")], vec![var("x")])]);
        assert!(e.macros.contains_key("m"));
    }

    #[test]
    fn test_expand_simple_variable() {
        let mut e = MacroExpander::new();
        e.register_macro(
            "id".into(),
            vec![rule(vec![var("x")], vec![var("x")])],
        );
        let result = e.expand_macro("id", &[Expression::IntLiteral(42)]).unwrap();
        match result {
            Statement::Expression(Expression::IntLiteral(42)) => {}
            _ => panic!("Expected IntLiteral(42)"),
        }
    }

    #[test]
    fn test_expand_macro_not_found() {
        let mut e = MacroExpander::new();
        assert!(e.expand_macro("missing", &[]).is_err());
    }

    #[test]
    fn test_expand_no_matching_rule() {
        let mut e = MacroExpander::new();
        // Rule expects 1 arg, but we pass 0
        e.register_macro("m".into(), vec![rule(vec![var("x")], vec![var("x")])]);
        assert!(e.expand_macro("m", &[]).is_err());
    }

    #[test]
    fn test_expand_multiple_rules_first_match() {
        let mut e = MacroExpander::new();
        e.register_macro("m".into(), vec![
            rule(vec![var("a"), var("b")], vec![var("a")]),
            rule(vec![var("x")], vec![var("x")]),
        ]);
        // Should match first rule with 2 args
        let result = e.expand_macro("m", &[Expression::IntLiteral(1), Expression::IntLiteral(2)]).unwrap();
        match result {
            Statement::Expression(Expression::IntLiteral(1)) => {}
            _ => panic!("Expected IntLiteral(1)"),
        }
    }

    #[test]
    fn test_expand_multiple_rules_fallback() {
        let mut e = MacroExpander::new();
        e.register_macro("m".into(), vec![
            rule(vec![var("a"), var("b")], vec![var("a")]),
            rule(vec![var("x")], vec![var("x")]),
        ]);
        // First rule fails (needs 2 args), second matches
        let result = e.expand_macro("m", &[Expression::IntLiteral(99)]).unwrap();
        match result {
            Statement::Expression(Expression::IntLiteral(99)) => {}
            _ => panic!("Expected IntLiteral(99)"),
        }
    }

    #[test]
    fn test_pattern_repetition() {
        let mut e = MacroExpander::new();
        e.register_macro(
            "rep".into(),
            vec![rule(
                vec![MacroToken::Repetition(vec![var("items")])],
                vec![var("items")],
            )],
        );
        let result = e.expand_macro(
            "rep",
            &[Expression::IntLiteral(1), Expression::IntLiteral(2), Expression::IntLiteral(3)],
        ).unwrap();
        // Should expand to first item in the repetition binding
        match result {
            Statement::Expression(Expression::IntLiteral(1)) => {}
            _ => panic!("Expected IntLiteral(1)"),
        }
    }

    #[test]
    fn test_pattern_repetition_empty() {
        let mut e = MacroExpander::new();
        // Repetition with no variable inside - bindings won't contain it
        e.register_macro(
            "rep".into(),
            vec![rule(
                vec![MacroToken::Repetition(vec![])],
                vec![var("missing")],
            )],
        );
        let result = e.expand_macro("rep", &[]).unwrap();
        // Falls through to default IntLiteral(0)
        match result {
            Statement::Expression(Expression::IntLiteral(0)) => {}
            _ => panic!("Expected default IntLiteral(0)"),
        }
    }

    #[test]
    fn test_pattern_literal_token() {
        let mut e = MacroExpander::new();
        e.register_macro(
            "m".into(),
            vec![rule(vec![lit(","), var("x")], vec![var("x")])],
        );
        let result = e.expand_macro(
            "m",
            &[Expression::StringLiteral(",".into()), Expression::IntLiteral(5)],
        ).unwrap();
        match result {
            Statement::Expression(Expression::IntLiteral(5)) => {}
            _ => panic!("Expected IntLiteral(5)"),
        }
    }

    #[test]
    fn test_expand_tokens_no_binding() {
        let mut e = MacroExpander::new();
        // Expansion references a variable not in bindings
        e.register_macro(
            "m".into(),
            vec![rule(vec![], vec![var("nonexistent")])],
        );
        let result = e.expand_macro("m", &[]).unwrap();
        match result {
            Statement::Expression(Expression::IntLiteral(0)) => {}
            _ => panic!("Expected default IntLiteral(0)"),
        }
    }

    #[test]
    fn test_expand_tokens_empty_expansion() {
        let mut e = MacroExpander::new();
        e.register_macro("m".into(), vec![rule(vec![], vec![])]);
        let result = e.expand_macro("m", &[]).unwrap();
        match result {
            Statement::Expression(Expression::IntLiteral(0)) => {}
            _ => panic!("Expected default IntLiteral(0)"),
        }
    }

    #[test]
    fn test_expand_tokens_literal_in_expansion() {
        let mut e = MacroExpander::new();
        // Expansion starts with a literal token, not a variable
        e.register_macro("m".into(), vec![rule(vec![], vec![lit("hello")])]);
        let result = e.expand_macro("m", &[]).unwrap();
        match result {
            Statement::Expression(Expression::IntLiteral(0)) => {}
            _ => panic!("Expected default IntLiteral(0)"),
        }
    }

    #[test]
    fn test_hygienic_name_generation() {
        let mut e = MacroExpander::new();
        let n1 = e.generate_hygienic_name("temp");
        let n2 = e.generate_hygienic_name("temp");
        let n3 = e.generate_hygienic_name("other");
        assert_eq!(n1, "temp__hygiene_1");
        assert_eq!(n2, "temp__hygiene_2");
        assert_eq!(n3, "other__hygiene_3");
        assert_ne!(n1, n2);
    }

    #[test]
    fn test_expand_with_string_literal_arg() {
        let mut e = MacroExpander::new();
        e.register_macro("m".into(), vec![rule(vec![var("s")], vec![var("s")])]);
        let result = e.expand_macro("m", &[Expression::StringLiteral("hello".into())]).unwrap();
        match result {
            Statement::Expression(Expression::StringLiteral(s)) => assert_eq!(s, "hello"),
            _ => panic!("Expected StringLiteral"),
        }
    }

    #[test]
    fn test_expand_with_bool_arg() {
        let mut e = MacroExpander::new();
        e.register_macro("m".into(), vec![rule(vec![var("b")], vec![var("b")])]);
        let result = e.expand_macro("m", &[Expression::BoolLiteral(true)]).unwrap();
        match result {
            Statement::Expression(Expression::BoolLiteral(true)) => {}
            _ => panic!("Expected BoolLiteral(true)"),
        }
    }
}
