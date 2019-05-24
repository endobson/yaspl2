#!/bin/bash
# Don't use 'set -e' as buildozer is silly
# set -e

buildozer fix "//...:*"
BUILDOZER_EXIT_CODE=$?
if [ $BUILDOZER_EXIT_CODE -ne 0 ] && [ $BUILDOZER_EXIT_CODE -ne 3 ]
then
  exit $BUILDOZER_EXIT_CODE
fi
find . -name BUILD -exec buildifier '{}' +
