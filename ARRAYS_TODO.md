# Arrays - Known Issue

## Status
Arrays are partially implemented but cause segfaults during compilation.

## What's Implemented
- ✅ AST support for arrays
- ✅ Parser support for array literals `[1, 2, 3]`
- ✅ Parser support for indexing `arr[0]`
- ✅ Type checker support
- ⚠️ Codegen implemented but crashes

## The Problem
The compiler segfaults when trying to compile array literals. The crash happens during LLVM IR generation, likely in one of these calls:
- `LLVMArrayType`
- `LLVMBuildAlloca`
- `LLVMBuildInBoundsGEP2`
- `LLVMBuildStore`

## Attempted Fixes
1. Used `LLVMArrayType` instead of `LLVMArrayType2`
2. Used `LLVMBuildInBoundsGEP2` with proper type parameter
3. Added array variable tracking to avoid type checking issues
4. Wrapped in unsafe blocks

## Next Steps to Debug
1. Add debug logging to see which LLVM call crashes
2. Check if builder/context/module are valid
3. Try simpler LLVM array creation (constant array instead of alloca)
4. Check LLVM version compatibility for these functions
5. Look at LLVM examples for proper array handling

## Workaround
For now, use manual memory allocation with pointers instead of arrays.

## Code Location
- Codegen: `/compiler/src/codegen/llvm_backend.rs` lines ~854-920
- Type checker: `/compiler/src/analyzer/type_checker.rs` line ~412
- Parser: `/compiler/src/parser/parser.rs` line ~701

## Test Cases
```kraken
// Simple inline array (crashes)
fn main() -> int {
    return [1, 2, 3][0];
}

// Array in variable (crashes)
fn main() -> int {
    let arr = [1, 2, 3];
    return arr[0];
}
```
