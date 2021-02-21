load("//libraries:yaspl.bzl", "yaspl_bootstrap_library", "yaspl_test")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_bootstrap_library(
    name = "yaspl-interpreter",
    srcs = ["yaspl-interpreter.yaspl"],
    deps = [
        "//libraries:top-level-name",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/yaspl:var",
        "//libraries/yaspl/languages:expanded-patterns-language",
    ],
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
