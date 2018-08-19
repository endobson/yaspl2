#!/bin/bash
bazel build //:missing_imports && buildozer -f `bazel info bazel-bin`/missing_imports.missing_imports
