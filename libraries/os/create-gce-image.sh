#!/bin/bash

set -e

bazel build //libraries/os:{make-exe,make-gpt}

rm -f tmp/prog.efi tmp/disk.raw tmp/compressed-image.tar.gz

bazel-bin/libraries/os/make-exe tmp/prog.efi
bazel-bin/libraries/os/make-gpt tmp/prog.efi tmp/disk.raw

gtar --format=oldgnu -Sczf tmp/compressed-image.tar.gz -C tmp disk.raw

gsutil cp tmp/compressed-image.tar.gz \
  gs://crucial-bloom-404/compressed-image.tar.gz

gcloud compute images create "image-$(date '+%Y-%m-%d-%H-%M')" \
  --project=crucial-bloom-404 \
  --source-uri=https://storage.googleapis.com/crucial-bloom-404/compressed-image.tar.gz \
  --storage-location=us \
  --guest-os-features="UEFI_COMPATIBLE"
