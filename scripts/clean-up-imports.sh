#~/bin/bash
bazel build //:clean_up_imports && \
patch -p0 < "$(bazel info bazel-bin)/clean_up_imports.clean_up_imports" && \
bazel build //:remove_unused_dependencies && \
buildozer -f `bazel info bazel-bin`/remove_unused_dependencies.remove_unused_dependencies
