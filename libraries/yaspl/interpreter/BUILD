load("//libraries:yaspl.bzl", "yaspl_bootstrap_library", "yaspl_test")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_bootstrap_library(
    name = "yaspl-interpreter",
    srcs = ["yaspl-interpreter.yaspl"],
)

yaspl_test(
    name = "yaspl-interpreter-test",
    srcs = ["yaspl-interpreter-test.yaspl"],
    deps = [
        ":yaspl-interpreter",
        "//libraries:yunit",
        "//libraries/data:list",
        "//libraries/data:maybe",
    ],
)

test_suite(
    name = "package_tests",
    testonly = 0,
)

filegroup(
    name = "package_binaries",
    srcs = [],
)
