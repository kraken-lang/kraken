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

    #[test]
    fn test_macro_expander_creation() {
        let expander = MacroExpander::new();
        assert_eq!(expander.macros.len(), 0);
    }

    #[test]
    fn test_register_macro() {
        let mut expander = MacroExpander::new();
        let rules = vec![MacroRule {
            pattern: vec![MacroToken::Variable("x".to_string())],
            expansion: vec![MacroToken::Variable("x".to_string())],
        }];
        expander.register_macro("test_macro".to_string(), rules);
        assert!(expander.macros.contains_key("test_macro"));
    }

    #[test]
    fn test_hygienic_name_generation() {
        let mut expander = MacroExpander::new();
        let name1 = expander.generate_hygienic_name("temp");
        let name2 = expander.generate_hygienic_name("temp");
        assert_ne!(name1, name2);
        assert!(name1.starts_with("temp__hygiene_"));
    }
}
