#~/bin/bash
bazel build //:clean_up_imports && \
patch -p0 < "$(bazel info bazel-bin)/clean_up_imports.clean_up_imports"
