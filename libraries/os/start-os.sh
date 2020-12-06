#!/bin/bash

cp bazel-yaspl2/external/ovmf_vars/file/downloaded tmp/vars
qemu-system-x86_64 -cpu host \
  --machine type=q35,accel=hvf \
  -drive if=pflash,format=raw,file=bazel-yaspl2/external/ovmf_code/file/downloaded,readonly \
  -drive if=pflash,format=raw,file=tmp/vars \
  -drive file=tmp/disk.img,format=raw,media=disk \
  -net none \
  --nographic -no-reboot
