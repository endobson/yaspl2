load("//libraries:yaspl.bzl", "yaspl_bootstrap_library")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_bootstrap_library(
    name = "errors",
    srcs = ["errors.yaspl"],
    deps = [
        "//libraries:source-to-intermediate-language",
        "//libraries:validator",
        "//libraries/yaspl/conversions:resolve-imports",
        "//libraries/yaspl/conversions:resolve-types",
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
