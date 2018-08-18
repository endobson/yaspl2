#!/bin/bash
bazel query "(kind('yaspl_binary', //...) - deps(//:all_binaries, 2))"
bazel query "(kind('yaspl_prim_test', //...) - tests(//:all_tests))"
