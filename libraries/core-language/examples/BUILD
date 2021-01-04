load("//libraries/core-language:core-language.bzl", "core_library")
load("//libraries:yaspl.bzl", "yaspl_test")

package(
    default_visibility = ["//visibility:public"],
)

core_library(
    name = "example-functions",
    srcs = ["example-functions.core"],
)

yaspl_test(
    name = "example-functions-test",
    srcs = ["example-functions-test.yaspl"],
    deps = [
        ":example-functions",
        "//libraries:yunit",
        "//libraries/data:list",
        "//libraries/data:maybe",
    ],
)

core_library(
    name = "fill-stack",
    srcs = ["fill-stack.core"],
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
