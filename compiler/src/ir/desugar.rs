//! IR Desugaring Passes
//!
//! Transforms complex syntactic constructs into simpler canonical forms:
//! - `for` loops → `while` loops
//! - `defer` statements → explicit cleanup at exit points
//!
//! NOTE: This module is reserved for future IR optimization passes.
#![allow(dead_code)]

use crate::ir::types::*;

/// Desugar pass that transforms IR constructs into simpler forms.
pub struct Desugar {
    next_value_id: u32,
    next_block_id: u32,
}

impl Desugar {
    pub fn new() -> Self {
        Self {
            next_value_id: 1000, // Start high to avoid conflicts
            next_block_id: 100,
        }
    }

    /// Run all desugar passes on a program.
    pub fn run(&mut self, program: &mut IrProgram) {
        for func in &mut program.functions {
            self.desugar_function(func);
        }
    }

    /// Desugar a single function.
    fn desugar_function(&mut self, func: &mut IrFunction) {
        // Collect defer statements for later insertion
        let defers = self.collect_defers(func);

        // Transform for loops to while loops in each block
        for block in &mut func.blocks {
            self.desugar_block(block);
        }

        // Insert defer cleanup at return points
        if !defers.is_empty() {
            self.insert_defers(func, &defers);
        }
    }

    /// Desugar instructions in a block.
    fn desugar_block(&mut self, block: &mut IrBlock) {
        let mut new_instructions = Vec::new();

        for instr in block.instructions.drain(..) {
            // For now, pass through all instructions
            // Full for→while would require AST-level transformation
            new_instructions.push(instr);
        }

        block.instructions = new_instructions;
    }

    /// Collect all defer statements from a function.
    fn collect_defers(&self, func: &IrFunction) -> Vec<DeferredCode> {
        let mut defers = Vec::new();

        for block in &func.blocks {
            for instr in &block.instructions {
                // Look for defer markers (convention: special call)
                if let IrInstruction::Call {
                    func: fn_name,
                    args,
                    ..
                } = instr
                {
                    if fn_name == "__defer_marker" {
                        // The deferred code would be encoded in args
                        // For now, just track that a defer exists
                        defers.push(DeferredCode {
                            instructions: vec![],
                            source_block: block.id,
                        });
                        let _ = args; // Suppress unused warning
                    }
                }
            }
        }

        defers
    }

    /// Insert deferred code at all function exit points.
    fn insert_defers(&mut self, func: &mut IrFunction, defers: &[DeferredCode]) {
        // Find all return instructions and insert defers before them
        for block in &mut func.blocks {
            let mut new_instructions = Vec::new();

            for instr in block.instructions.drain(..) {
                if matches!(instr, IrInstruction::Return { .. }) {
                    // Insert defers in LIFO order before return
                    for defer in defers.iter().rev() {
                        new_instructions.extend(defer.instructions.clone());
                    }
                }
                new_instructions.push(instr);
            }

            block.instructions = new_instructions;
        }
    }

    #[allow(dead_code)]
    fn alloc_value(&mut self) -> ValueId {
        let id = ValueId(self.next_value_id);
        self.next_value_id += 1;
        id
    }

    #[allow(dead_code)]
    fn alloc_block(&mut self) -> BlockId {
        let id = BlockId(self.next_block_id);
        self.next_block_id += 1;
        id
    }
}

impl Default for Desugar {
    fn default() -> Self {
        Self::new()
    }
}

/// Represents deferred code to be executed at function exit.
#[derive(Debug, Clone)]
pub struct DeferredCode {
    /// Instructions to execute.
    pub instructions: Vec<IrInstruction>,
    /// Block where the defer was declared.
    pub source_block: BlockId,
}

/// Transform a for loop into a while loop.
///
/// ```text
/// for (init; cond; inc) { body }
/// ```
///
/// Becomes:
///
/// ```text
/// {
///     init;
///     while (cond) {
///         body;
///         inc;
///     }
/// }
/// ```
#[derive(Debug)]
pub struct ForToWhile {
    /// Initializer instructions.
    pub init: Vec<IrInstruction>,
    /// Condition value.
    pub condition: IrValue,
    /// Increment instructions.
    pub increment: Vec<IrInstruction>,
    /// Body instructions.
    pub body: Vec<IrInstruction>,
}

impl ForToWhile {
    /// Transform to while loop blocks.
    pub fn to_while_blocks(
        &self,
        entry_id: BlockId,
        body_id: BlockId,
        exit_id: BlockId,
    ) -> Vec<IrBlock> {
        let mut blocks = Vec::new();

        // Entry block: init + branch to condition check
        let mut entry = IrBlock::new(entry_id, "for_init".to_string());
        entry.instructions.extend(self.init.clone());
        entry.instructions.push(IrInstruction::CondBranch {
            cond: self.condition.clone(),
            then_block: body_id,
            else_block: exit_id,
        });
        blocks.push(entry);

        // Body block: body + increment + branch back to condition
        let mut body = IrBlock::new(body_id, "for_body".to_string());
        body.instructions.extend(self.body.clone());
        body.instructions.extend(self.increment.clone());
        body.instructions.push(IrInstruction::CondBranch {
            cond: self.condition.clone(),
            then_block: body_id,
            else_block: exit_id,
        });
        blocks.push(body);

        // Exit block (empty, control continues)
        let exit = IrBlock::new(exit_id, "for_exit".to_string());
        blocks.push(exit);

        blocks
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_desugar_empty_program() {
        let mut program = IrProgram::new();
        let mut desugar = Desugar::new();
        desugar.run(&mut program);
        assert!(program.functions.is_empty());
    }

    #[test]
    fn test_desugar_simple_function() {
        let mut program = IrProgram::new();
        let mut func = IrFunction::new("test".to_string(), vec![], IrType::Void, false);
        let mut block = IrBlock::new(BlockId(0), "entry".to_string());
        block
            .instructions
            .push(IrInstruction::Return { value: None });
        func.blocks.push(block);
        program.functions.push(func);

        let mut desugar = Desugar::new();
        desugar.run(&mut program);

        assert_eq!(program.functions.len(), 1);
        assert_eq!(program.functions[0].blocks.len(), 1);
    }

    #[test]
    fn test_for_to_while_transformation() {
        let for_loop = ForToWhile {
            init: vec![IrInstruction::Alloca {
                dest: ValueId(0),
                ty: IrType::Int,
                name: "i".to_string(),
            }],
            condition: IrValue::ConstBool(true),
            increment: vec![],
            body: vec![],
        };

        let blocks = for_loop.to_while_blocks(BlockId(0), BlockId(1), BlockId(2));
        assert_eq!(blocks.len(), 3);
        assert_eq!(blocks[0].name, "for_init");
        assert_eq!(blocks[1].name, "for_body");
        assert_eq!(blocks[2].name, "for_exit");
    }
}
