#!/bin/bash
set -e

bazel build //examples/... //libraries/... --experimental_action_listener=//libraries:yaspl-lint
find bazel-out/local-fastbuild/extra_actions/libraries/yaspl-lint-action -name '*.lint' -print0 | xargs -0 -- cat
