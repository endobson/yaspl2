load("//libraries:yaspl.bzl", "yaspl_bootstrap_library")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_bootstrap_library(
    name = "errors",
    srcs = ["errors.yaspl"],
    deps = [
        "//libraries:resolved-imports-language",
        "//libraries:resolved-types-language",
        "//libraries:source-to-intermediate-language",
        "//libraries:validator",
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
