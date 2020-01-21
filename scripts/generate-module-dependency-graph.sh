#!/bin/bash

if [ $# -ne 1 ]; then
  echo 1>&2 "Usage: $0 output-file.svg"
  exit 1
fi

bazel build //meta:module_dependencies && \
tred bazel-bin/meta/module_dependencies.module-dependency-graph | \
  dot -Tsvg -o $1
