#!/bin/bash
# Build Kraken runtime library

set -e

cd "$(dirname "$0")"

echo "Building Kraken runtime library..."

# Compile C runtime
clang -c -O2 -fPIC kraken_string.c -o kraken_string.o
clang -c -O2 -fPIC kraken_stdlib.c -o kraken_stdlib.o
clang -c -O2 -fPIC kraken_safety.c -o kraken_safety.o
clang -c -O2 -fPIC kraken_collections.c -o kraken_collections.o
clang -c -O2 -fPIC kraken_ffi_safety.c -o kraken_ffi_safety.o
clang -c -O2 -fPIC kraken_struct_layout.c -o kraken_struct_layout.o
clang -c -O2 -fPIC kraken_union_runtime.c -o kraken_union_runtime.o
clang -c -O2 -fPIC kraken_variadic.c -o kraken_variadic.o

# Create static library
ar rcs libkraken_runtime.a \
    kraken_string.o \
    kraken_stdlib.o \
    kraken_safety.o \
    kraken_collections.o \
    kraken_ffi_safety.o \
    kraken_struct_layout.o \
    kraken_union_runtime.o \
    kraken_variadic.o

echo "Runtime library built: libkraken_runtime.a"
