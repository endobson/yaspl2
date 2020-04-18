#!/bin/bash

if [[ -s meta/validate_ir.validate_ir ]]
then
  cat meta/validate_ir.validate_ir >&2
  exit 1
fi
