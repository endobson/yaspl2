#!/bin/bash
bazel build //prebuilt:setup_prebuilt_binaries && \
$(bazel info bazel-genfiles)/prebuilt/setup_prebuilt_binaries
