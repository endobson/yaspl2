#!/bin/bash
bazel build //:lint.lint && cat `bazel info bazel-bin`/lint.lint
