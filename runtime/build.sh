#!/bin/bash
# Build Kraken runtime library
# Cross-platform: works on Linux, macOS, and Windows (Git Bash)

set -e

cd "$(dirname "$0")"

# Detect archiver: ar (Unix) or llvm-ar (Windows via LLVM package)
if command -v ar &> /dev/null; then
    AR=ar
elif command -v llvm-ar &> /dev/null; then
    AR=llvm-ar
else
    echo "Error: neither ar nor llvm-ar found in PATH"
    exit 1
fi

echo "Building Kraken runtime library (archiver: $AR)..."

SOURCES=(
    kraken_string
    kraken_stdlib
    kraken_safety
    kraken_collections
    kraken_ffi_safety
    kraken_struct_layout
    kraken_union_runtime
    kraken_variadic
)

# Compile C runtime
for src in "${SOURCES[@]}"; do
    clang -c -O2 -fPIC "${src}.c" -o "${src}.o"
done

# Create static library
OBJECTS=()
for src in "${SOURCES[@]}"; do
    OBJECTS+=("${src}.o")
done

$AR rcs libkraken_runtime.a "${OBJECTS[@]}"

echo "Runtime library built: libkraken_runtime.a"
