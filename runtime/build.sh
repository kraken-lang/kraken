#!/bin/bash
# Build Kraken runtime library

set -e

cd "$(dirname "$0")"

echo "Building Kraken runtime library..."

# Compile C runtime
clang -c -O2 -fPIC kraken_string.c -o kraken_string.o
clang -c -O2 -fPIC kraken_stdlib.c -o kraken_stdlib.o
clang -c -O2 -fPIC kraken_safety.c -o kraken_safety.o

# Create static library
ar rcs libkraken_runtime.a kraken_string.o kraken_stdlib.o kraken_safety.o

echo "Runtime library built: libkraken_runtime.a"
