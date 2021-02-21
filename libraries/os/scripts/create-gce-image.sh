#!/bin/bash

set -e
set -x

rm -f tmp/kernel.efi tmp/disk.raw tmp/compressed-image.tar.gz

bazel build //libraries/os:kernel.efi \
  --platforms=//constraints/platforms:yasplos_x86_64

cp -f bazel-bin/libraries/os/kernel.efi tmp/kernel.efi

bazel build //libraries/os:make-gpt

bazel-bin/libraries/os/make-gpt tmp/kernel.efi tmp/disk.raw

gtar --format=oldgnu -Sczf tmp/compressed-image.tar.gz -C tmp disk.raw

gsutil cp tmp/compressed-image.tar.gz \
  gs://crucial-bloom-404/compressed-image.tar.gz

gcloud compute images create "image-$(date '+%Y-%m-%d-%H-%M')" \
  --project=crucial-bloom-404 \
  --source-uri=https://storage.googleapis.com/crucial-bloom-404/compressed-image.tar.gz \
  --storage-location=us \
  --guest-os-features="UEFI_COMPATIBLE"
