#!/bin/bash

cp bazel-yaspl2/external/ovmf_vars/file/downloaded tmp/vars
qemu-system-x86_64 -cpu host \
  --machine type=q35,accel=hvf \
  -drive if=pflash,format=raw,file=bazel-yaspl2/external/ovmf_code/file/downloaded,readonly \
  -drive if=pflash,format=raw,file=tmp/vars \
  -drive file=tmp/disk.img,format=raw,media=disk \
  -nic user,id=mynet,model=virtio-net-pci,hostfwd=udp::10007-:7 \
  -object filter-dump,id=dump,netdev=mynet,file=tmp/network.pcap \
  --nographic -no-reboot --no-shutdown
