#!/bin/bash
set -e

BAZEL_OUT="$(bazel info output_path)"
bazel build //bootstrap:compiler_binaries
cp "${BAZEL_OUT}"/host/bin/bootstrap/bootstrap_library_compiler bootstrap/saved_library_compiler
chmod ug+w bootstrap/saved_library_compiler
cp "${BAZEL_OUT}"/host/bin/bootstrap/bootstrap_main_stub bootstrap/saved_main_stub
chmod ug+w bootstrap/saved_main_stub
cp "${BAZEL_OUT}"/host/bin/bootstrap/bootstrap_linker bootstrap/saved_linker
chmod ug+w bootstrap/saved_linker
