#!/usr/bin/env bash

set -e

bazel build //libraries/os:{examples,msabi-runtime.o} \
  --platforms=//constraints/platforms:yasplos_x86_64

cp -f bazel-bin/libraries/os/examples.o tmp/os/examples.obj
cp -f bazel-bin/libraries/os/msabi-runtime.o tmp/os/msabi-runtime.obj

bazel build //libraries:linker 

rm -f tmp/kernel.efi
bazel-bin/libraries/linker yasplos tmp/kernel.efi \
  tmp/os/{hello-c,primitives,serial,strings,descriptor_tables,scheduler,examples,msabi-runtime}.obj

