#!/usr/bin/env bash
gcloud compute instances create \
  instance-$1 --zone=us-west1-a --machine-type=f1-micro \
  --no-service-account --no-scopes --image=$1
