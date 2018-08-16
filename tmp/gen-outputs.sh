#!/bin/bash
rm -f output*.o && \
bazel build //examples:elf-file && \
`bazel info bazel-bin`/examples/elf-file output1.o output2.o output3.o output4.o && \
xxd output1.o >| output1.xxd && \
xxd output2.o >| output2.xxd && \
xxd output3.o >| output3.xxd && \
xxd output4.o >| output4.xxd
