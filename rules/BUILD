load("//libraries:yaspl.bzl", "yaspl_binary")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_binary(
    name = "embedder",
    srcs = ["embedder-main.yaspl"],
    deps = [
        "//libraries:byte-escapes",
        "//libraries:io",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//tools:command-line",
    ],
)

test_suite(
    name = "package_tests",
    testonly = 0,
)

filegroup(
    name = "package_binaries",
    srcs = [
        ":embedder",
    ],
)
