#!/bin/bash
rm -rf tmp/bazel && \
mkdir tmp/bazel && \
bazel build //libraries/prim-language/examples/... --cpu=k8 && \
cp -f -R bazel-bin/libraries/prim-language/examples/* tmp/bazel/ && \
rm -f tmp/bazel/call1_lib.o
for prog in `find tmp/bazel -type file -name '*.o' | sed -e 's/.o$//'`;
do
  xxd "${prog}" "${prog}.xxd";
done;
