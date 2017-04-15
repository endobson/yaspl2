#!/bin/bash
set -e

bazel build //tools:rewrite-imports
for file in $(git st --porcelain | cut -c 4- | grep '^.*\.yaspl$')
do
  bazel-bin/tools/rewrite-imports $file
done
