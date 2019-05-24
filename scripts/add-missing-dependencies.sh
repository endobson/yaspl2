#!/bin/bash
bazel build //meta:missing_dependencies && \
buildozer -f `bazel info bazel-bin`/meta/missing_dependencies.missing_dependencies
