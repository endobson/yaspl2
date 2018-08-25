#!/bin/bash
set -e


BAZEL_OUT="$(bazel info output_path)"
bazel build //prebuilt:prebuilt_binaries
mkdir -p prebuilt/saved
cp "${BAZEL_OUT}"/host/bin/bootstrap/bootstrap_library_compiler prebuilt/saved/library_compiler
chmod ug+w prebuilt/saved/library_compiler
cp "${BAZEL_OUT}"/host/bin/bootstrap/bootstrap_main_stub prebuilt/saved/main_stub
chmod ug+w prebuilt/saved/main_stub
cp "${BAZEL_OUT}"/host/bin/bootstrap/bootstrap_linker prebuilt/saved/linker
chmod ug+w prebuilt/saved/linker
cp "${BAZEL_OUT}"/host/bin/bootstrap/bootstrap_prim_language_library_compiler \
  prebuilt/saved/prim_language_library_compiler
chmod ug+w prebuilt/saved/prim_language_library_compiler
