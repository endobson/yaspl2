#!/bin/bash
bazel query --noshow_progress \
 "(kind('yaspl_prim_binary', //...) - deps(//:all_binaries, 2)) + "\
 "(let all_test_targets = kind('_test', //...) in \$all_test_targets - "\
    "attr(tags, manual, \$all_test_targets)  - tests(//:all_tests))"
