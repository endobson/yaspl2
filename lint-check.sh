#!/bin/bash
set -e

bazel build //tools/... //examples/... //libraries/... --aspects tools/yaspl-lint.bzl%yaspl_lint \
  --output_groups=lint
find bazel-out/local-fastbuild/bin/ -name '*.lint' -print0 | xargs -0 -- cat
