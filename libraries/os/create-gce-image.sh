#!/bin/bash

set -e

gcloud compute images create $1 \
  --project=crucial-bloom-404 \
  --source-uri=https://storage.googleapis.com/crucial-bloom-404/compressed-image.tar.gz \
  --storage-location=us \
  --guest-os-features="UEFI_COMPATIBLE"
