#!/bin/bash
set -e

bazel build //bootstrap:compiler_binaries
cp bazel-out/host/bin/bootstrap/bootstrap_library_compiler bootstrap/saved_library_compiler
chmod ug+w bootstrap/saved_library_compiler
cp bazel-out/host/bin/bootstrap/bootstrap_main_stub bootstrap/saved_main_stub
chmod ug+w bootstrap/saved_main_stub
