#!/usr/bin/env bash

set -e

bazel build //libraries/os:kernel.efi \
  --platforms=//constraints/platforms:yasplos_x86_64

cp -f bazel-bin/libraries/os/kernel.efi tmp/kernel.efi
