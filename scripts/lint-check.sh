#!/bin/bash
bazel build //meta:lint.lint && cat `bazel info bazel-bin`/meta/lint.lint
