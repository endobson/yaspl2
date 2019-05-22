load("//libraries:yaspl.bzl", "yaspl_library")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_library(
    name = "http-response",
    srcs = ["http-response.yaspl"],
    deps = [],
)

yaspl_library(
    name = "http-responses",
    srcs = ["http-responses.yaspl"],
    deps = [
        ":http-response",
        "//libraries/formats:xml",
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
