#!/bin/bash
set -e

bazel build //tools:rewrite-imports
for file in $(git st --porcelain | cut -c 4- | sed -e "s/.* //" | grep '^.*\.yaspl$')
do
  exit_code=0
  bazel-bin/tools/rewrite-imports $file || exit_code=$?
  if [ $exit_code -ne 0 ]; then
    echo "Error rewriting '$file'"
  fi;
done
