#!/bin/bash
# Script to find all files that need updating for Unsafe statement and RawPointer type

echo "Finding files with Statement match patterns..."
grep -r "Statement::Defer" compiler/src --include="*.rs" | cut -d: -f1 | sort -u

echo ""
echo "Finding files with Type match patterns..."
grep -r "Type::Pointer" compiler/src --include="*.rs" | cut -d: -f1 | sort -u

echo ""
echo "Finding FunctionDeclaration patterns..."
grep -r "FunctionDeclaration {" compiler/src --include="*.rs" | grep -v "//\|test" | cut -d: -f1 | sort -u
