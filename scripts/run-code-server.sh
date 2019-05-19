#!/bin/bash

if [ $# -ne 1 ]; then
  echo 1>&2 "Usage: $0 bazel-target"
  exit 1
fi

bazel build libraries/code-http-server && \
bazel-bin/libraries/code-http-server 127.0.0.1 8080 \
`bazel query "filter('.src_dep', deps($1))" | \
   sed -e 's|^//||' -e 's|:|/|' -e 's|.src_dep$||' | \
   tail -r`
