#!/bin/bash
# Build Kraken runtime library

set -e

cd "$(dirname "$0")"

echo "Building Kraken runtime library..."

# Compile C runtime
clang -c -O2 -fPIC kraken_string.c -o kraken_string.o

# Create static library
ar rcs libkraken_runtime.a kraken_string.o

echo "Runtime library built: libkraken_runtime.a"
