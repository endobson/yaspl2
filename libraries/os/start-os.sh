#!/bin/bash

qemu-system-x86_64 -cpu qemu64 \
  -drive file=tmp/disk.img,format=raw,index=0,media=disk \
  -drive if=pflash,format=raw,unit=0,file=bazel-yaspl2/external/ovmf_code/file/downloaded,readonly=on \
  -net none --nographic
