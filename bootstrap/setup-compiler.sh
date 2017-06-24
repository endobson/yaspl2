#!/bin/bash
set -e

bazel build //bootstrap:compiler_binaries
cp bazel-out/darwin_x86_64-fastbuild/bin/bootstrap/bootstrap_library_compiler bootstrap/saved_library_compiler
chmod ug+w bootstrap/saved_library_compiler
cp bazel-out/darwin_x86_64-fastbuild/bin/bootstrap/bootstrap_main_stub bootstrap/saved_main_stub
chmod ug+w bootstrap/saved_main_stub
cp bazel-out/darwin_x86_64-fastbuild/bin/bootstrap/bootstrap_linker bootstrap/saved_linker
chmod ug+w bootstrap/saved_linker
