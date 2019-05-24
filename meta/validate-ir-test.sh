#!/bin/bash

if [[ -s validate_ir.validate_ir ]]
then
  cat validate_ir.validate_ir >&2
  exit 1
fi
