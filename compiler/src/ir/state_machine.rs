//! Async State Machine Lowering
//!
//! Transforms async functions into state machines at the IR level.
//! Each await point becomes a state transition.
//!
//! ## Transformation
//!
//! ```text
//! async fn foo() -> int {
//!     let x = await bar();
//!     let y = await baz();
//!     return x + y;
//! }
//! ```
//!
//! Becomes a state machine struct and poll function:
//!
//! ```text
//! struct FooStateMachine {
//!     state: int,      // Current state (0 = initial, 1 = after bar, 2 = after baz, -1 = done)
//!     x: int,          // Captured local
//!     y: int,          // Captured local
//!     bar_future: ..., // Nested future
//!     baz_future: ..., // Nested future
//! }
//!
//! fn foo_poll(sm: *FooStateMachine) -> PollResult {
//!     match sm.state {
//!         0 => { /* call bar, transition to state 1 */ }
//!         1 => { /* poll bar, if ready: store x, call baz, transition to state 2 */ }
//!         2 => { /* poll baz, if ready: store y, return x + y, state = -1 */ }
//!     }
//! }
//! ```

use crate::ir::types::*;
use std::collections::HashMap;

/// Represents an await point in an async function.
#[derive(Debug, Clone)]
pub struct AwaitPoint {
    /// Unique ID for this await point (becomes a state number).
    pub id: u32,
    /// The future expression being awaited.
    pub future_expr: IrValue,
    /// The destination value ID for the await result.
    pub result_dest: Option<ValueId>,
    /// The type of the awaited value.
    pub result_ty: IrType,
}

/// Captures a local variable that needs to be preserved across await points.
#[derive(Debug, Clone)]
pub struct CapturedLocal {
    pub name: String,
    pub ty: IrType,
    pub value_id: ValueId,
}

/// State machine representation of an async function.
#[derive(Debug, Clone)]
pub struct AsyncStateMachine {
    /// Original function name.
    pub name: String,
    /// State machine struct name (e.g., "FooStateMachine").
    pub struct_name: String,
    /// Poll function name (e.g., "foo_poll").
    pub poll_fn_name: String,
    /// Parameters from the original async function.
    pub params: Vec<IrParam>,
    /// Return type of the async function.
    pub return_type: IrType,
    /// Await points (each becomes a state).
    pub await_points: Vec<AwaitPoint>,
    /// Local variables captured across await boundaries.
    pub captured_locals: Vec<CapturedLocal>,
    /// State transitions: from_state -> (condition, to_state, actions).
    pub transitions: Vec<StateTransition>,
}

/// A state transition in the state machine.
#[derive(Debug, Clone)]
pub struct StateTransition {
    /// Source state number.
    pub from_state: u32,
    /// Target state number.
    pub to_state: u32,
    /// Instructions to execute during this transition.
    pub instructions: Vec<IrInstruction>,
}

/// Poll result enum values.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PollResult {
    /// The future is still pending.
    Pending = 0,
    /// The future completed with a value.
    Ready = 1,
}

/// Analyzes an async function and extracts await points.
pub struct AsyncAnalyzer {
    await_points: Vec<AwaitPoint>,
    captured_locals: HashMap<String, CapturedLocal>,
    next_await_id: u32,
}

impl AsyncAnalyzer {
    pub fn new() -> Self {
        Self {
            await_points: Vec::new(),
            captured_locals: HashMap::new(),
            next_await_id: 0,
        }
    }

    /// Analyze an IR function to find await points and captured locals.
    pub fn analyze(&mut self, func: &IrFunction) -> AsyncStateMachine {
        // Scan all blocks for await-like calls
        for block in &func.blocks {
            for instr in &block.instructions {
                self.analyze_instruction(instr);
            }
        }

        // Collect captured locals (variables used after an await)
        let captured: Vec<CapturedLocal> = self.captured_locals.values().cloned().collect();

        AsyncStateMachine {
            name: func.name.clone(),
            struct_name: format!("{}StateMachine", capitalize(&func.name)),
            poll_fn_name: format!("{}_poll", func.name),
            params: func.params.clone(),
            return_type: func.return_type.clone(),
            await_points: self.await_points.clone(),
            captured_locals: captured,
            transitions: Vec::new(), // Filled in during lowering
        }
    }

    fn analyze_instruction(&mut self, instr: &IrInstruction) {
        // Look for calls that represent await points
        // In the IR, an await would be represented as a special call or marker
        if let IrInstruction::Call { dest, func, args, ret_ty } = instr {
            // Check if this is an await marker (convention: functions ending in "_await")
            if func.ends_with("_await") || func == "await" {
                let await_point = AwaitPoint {
                    id: self.next_await_id,
                    future_expr: args.first().cloned().unwrap_or(IrValue::Null),
                    result_dest: *dest,
                    result_ty: ret_ty.clone(),
                };
                self.await_points.push(await_point);
                self.next_await_id += 1;
            }
        }

        // Track allocas as potential captured locals
        if let IrInstruction::Alloca { dest, ty, name } = instr {
            self.captured_locals.insert(
                name.clone(),
                CapturedLocal {
                    name: name.clone(),
                    ty: ty.clone(),
                    value_id: *dest,
                },
            );
        }
    }
}

impl Default for AsyncAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

/// Transforms an async function into a state machine.
pub struct StateMachineLowering {
    next_value_id: u32,
    next_block_id: u32,
}

impl StateMachineLowering {
    pub fn new() -> Self {
        Self {
            next_value_id: 0,
            next_block_id: 0,
        }
    }

    /// Lower an async function to a state machine struct and poll function.
    pub fn lower(&mut self, func: &IrFunction) -> (IrStruct, IrFunction) {
        let mut analyzer = AsyncAnalyzer::new();
        let sm = analyzer.analyze(func);

        let state_struct = self.generate_state_struct(&sm);
        let poll_fn = self.generate_poll_function(&sm, func);

        (state_struct, poll_fn)
    }

    /// Generate the state machine struct.
    fn generate_state_struct(&self, sm: &AsyncStateMachine) -> IrStruct {
        let mut fields = vec![
            ("state".to_string(), IrType::Int), // Current state
        ];

        // Add captured locals as fields
        for local in &sm.captured_locals {
            fields.push((local.name.clone(), local.ty.clone()));
        }

        // Add fields for nested futures (simplified: store as opaque pointers)
        for (i, _await_point) in sm.await_points.iter().enumerate() {
            fields.push((format!("future_{}", i), IrType::Bytes));
        }

        IrStruct {
            name: sm.struct_name.clone(),
            fields,
            is_public: false,
        }
    }

    /// Generate the poll function for the state machine.
    fn generate_poll_function(&mut self, sm: &AsyncStateMachine, _original: &IrFunction) -> IrFunction {
        let sm_ptr_param = IrParam {
            name: "sm".to_string(),
            ty: IrType::Pointer(Box::new(IrType::Struct(sm.struct_name.clone()))),
        };

        let mut poll_fn = IrFunction::new(
            sm.poll_fn_name.clone(),
            vec![sm_ptr_param],
            IrType::Int, // Returns poll result (0 = pending, 1 = ready)
            true,
        );

        // Create entry block that dispatches based on state
        let entry_block = self.create_dispatch_block(sm);
        poll_fn.blocks.push(entry_block);

        // Create a block for each state
        for (i, _await_point) in sm.await_points.iter().enumerate() {
            let state_block = self.create_state_block(sm, i as u32);
            poll_fn.blocks.push(state_block);
        }

        // Create done block
        let done_block = self.create_done_block();
        poll_fn.blocks.push(done_block);

        poll_fn
    }

    /// Create the dispatch block that switches on state.
    fn create_dispatch_block(&mut self, sm: &AsyncStateMachine) -> IrBlock {
        let block_id = BlockId(self.alloc_block_id());
        let mut block = IrBlock::new(block_id, "entry".to_string());

        // Load state field from state machine
        let sm_ptr = self.alloc_value();
        let state_val = self.alloc_value();

        block.instructions.push(IrInstruction::Alloca {
            dest: sm_ptr,
            ty: IrType::Pointer(Box::new(IrType::Struct(sm.struct_name.clone()))),
            name: "sm_ptr".to_string(),
        });

        block.instructions.push(IrInstruction::Load {
            dest: state_val,
            ptr: IrValue::Variable("sm".to_string()),
            ty: IrType::Int,
        });

        // For now, just return pending (0) as placeholder
        // Full implementation would have switch/branch based on state
        block.instructions.push(IrInstruction::Return {
            value: Some(IrValue::ConstInt(PollResult::Pending as i64)),
        });

        block
    }

    /// Create a block for handling a specific state.
    fn create_state_block(&mut self, _sm: &AsyncStateMachine, state_num: u32) -> IrBlock {
        let block_id = BlockId(self.alloc_block_id());
        let mut block = IrBlock::new(block_id, format!("state_{}", state_num));

        // Placeholder: each state block would:
        // 1. Poll the corresponding future
        // 2. If pending, return Pending
        // 3. If ready, store result, transition to next state, continue

        block.instructions.push(IrInstruction::Return {
            value: Some(IrValue::ConstInt(PollResult::Pending as i64)),
        });

        block
    }

    /// Create the done block (state = -1).
    fn create_done_block(&mut self) -> IrBlock {
        let block_id = BlockId(self.alloc_block_id());
        let mut block = IrBlock::new(block_id, "done".to_string());

        block.instructions.push(IrInstruction::Return {
            value: Some(IrValue::ConstInt(PollResult::Ready as i64)),
        });

        block
    }

    fn alloc_value(&mut self) -> ValueId {
        let id = ValueId(self.next_value_id);
        self.next_value_id += 1;
        id
    }

    fn alloc_block_id(&mut self) -> u32 {
        let id = self.next_block_id;
        self.next_block_id += 1;
        id
    }
}

impl Default for StateMachineLowering {
    fn default() -> Self {
        Self::new()
    }
}

/// Capitalize the first letter of a string.
fn capitalize(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(first) => first.to_uppercase().chain(chars).collect(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_async_analyzer_empty_function() {
        let func = IrFunction::new("test".to_string(), vec![], IrType::Void, false);
        let mut analyzer = AsyncAnalyzer::new();
        let sm = analyzer.analyze(&func);
        
        assert_eq!(sm.name, "test");
        assert_eq!(sm.struct_name, "TestStateMachine");
        assert_eq!(sm.poll_fn_name, "test_poll");
        assert!(sm.await_points.is_empty());
    }

    #[test]
    fn test_state_machine_lowering() {
        let func = IrFunction::new("async_foo".to_string(), vec![], IrType::Int, true);
        let mut lowering = StateMachineLowering::new();
        let (state_struct, poll_fn) = lowering.lower(&func);

        assert_eq!(state_struct.name, "Async_fooStateMachine");
        assert_eq!(poll_fn.name, "async_foo_poll");
        assert!(!poll_fn.blocks.is_empty());
    }

    #[test]
    fn test_capitalize() {
        assert_eq!(capitalize("foo"), "Foo");
        assert_eq!(capitalize(""), "");
        assert_eq!(capitalize("a"), "A");
    }
}
