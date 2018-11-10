#!/bin/bash
rm -rf tmp/bazel-obj && \
mkdir tmp/bazel-obj && \
bazel build //libraries/prim-language/examples/... --cpu=k8 && \
cp -f bazel-bin/libraries/prim-language/examples/*.o tmp/bazel-obj && \
rm -f tmp/bazel-obj/call1_lib.o
