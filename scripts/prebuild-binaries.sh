#!/bin/bash
bazel build //prebuilt:setup_prebuilt_binaries \
  --define yaspl_bootstrap=true && \
$(bazel info bazel-genfiles)/prebuilt/setup_prebuilt_binaries
