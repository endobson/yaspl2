#!/bin/bash

if [[ -s meta/lint.lint ]]
then
  cat meta/lint.lint >&2
  exit 1
fi
