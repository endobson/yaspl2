#!/bin/bash

#  -drive file=tmp/disk.img,format=raw,media=disk \
qemu-system-x86_64 -cpu qemu64 \
  -drive if=pflash,format=raw,file=bazel-yaspl2/external/ovmf_code/file/downloaded,readonly \
  -drive if=pflash,format=raw,file=bazel-yaspl2/external/ovmf_vars/file/downloaded \
  -net none --nographic -no-reboot
