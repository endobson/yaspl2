#!/bin/bash
set -e
WORKSPACE=`bazel info workspace`
rm -rf $WORKSPACE/tmp/bazel
mkdir $WORKSPACE/tmp/bazel
bazel build //libraries/prim-language/examples/... --cpu=k8
cp -f -R $WORKSPACE/bazel-bin/libraries/prim-language/examples/* $WORKSPACE/tmp/bazel/
rm -f $WORKSPACE/tmp/bazel/call1_lib.o
for prog in `find $WORKSPACE/tmp/bazel -type file -name '*.o' | sed -e 's/.o$//'`;
do
  xxd "${prog}" "${prog}.xxd";
  diff -u "${prog}.xxd" "$(dirname $(dirname ${prog}))/bazel-obj/$(basename ${prog}).xxd";
done;
