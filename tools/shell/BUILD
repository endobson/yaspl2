load("//libraries:yaspl.bzl", "yaspl_binary")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_binary(
    name = "shell",
    srcs = ["shell-main.yaspl"],
    deps = [
        "//libraries:io",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
    ],
)

test_suite(
    name = "package_tests",
    testonly = 0,
)

filegroup(
    name = "package_binaries",
    srcs = [
        ":shell",
    ],
)
