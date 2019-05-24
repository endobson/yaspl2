#~/bin/bash
bazel build //meta:clean_up_imports && \
patch -p0 < "$(bazel info bazel-bin)/meta/clean_up_imports.clean_up_imports" && \
bazel build //meta:remove_unused_dependencies && \
buildozer -f `bazel info bazel-bin`/meta/remove_unused_dependencies.remove_unused_dependencies
