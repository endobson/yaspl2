#!/bin/bash
bazel build //:missing_dependencies && \
buildozer -f `bazel info bazel-bin`/missing_dependencies.missing_dependencies
