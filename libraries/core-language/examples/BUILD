load("//libraries/core-language:core-language.bzl", "core_library")
load("//libraries:yaspl.bzl", "yaspl_test")

package(
    default_visibility = ["//visibility:public"],
)

core_library(
    name = "fill-stack",
    srcs = ["fill-stack.core"],
)

yaspl_test(
    name = "fill-stack-test",
    srcs = ["fill-stack-test.yaspl"],
    deps = [
        ":fill-stack",
        "//libraries:yunit",
        "//libraries/data:list",
        "//libraries/data:maybe",
    ],
)

filegroup(
    name = "package_binaries",
    srcs = [
    ],
)

test_suite(
    name = "package_tests",
    testonly = 0,
)
