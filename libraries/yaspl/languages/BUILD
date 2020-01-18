load(
    "//libraries:yaspl.bzl",
    "yaspl_bootstrap_library",
)

package(
    default_visibility = ["//visibility:public"],
)

yaspl_bootstrap_library(
    name = "source-language",
    srcs = ["source-language.yaspl"],
    deps = [
        "//libraries:module-name",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
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
