#!/bin/bash
set -e

bazel build //tools/... //examples/... //libraries/... --aspects tools/yaspl-analysis.bzl%yaspl_analysis \
  --output_groups=analysis
find `bazel info bazel-bin` -name '*.analysis' -print0 | xargs -0 -- cat
