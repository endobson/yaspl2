#!/bin/bash
bazel query "(kind('yaspl_prim_binary', //...) - deps(//:all_binaries, 2))"
bazel query "(kind('_test', //...) - tests(//:all_tests))"
