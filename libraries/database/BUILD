load("//libraries:yaspl.bzl", "yaspl_library", "yaspl_test")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_library(
    name = "relation-structures",
    srcs = ["relation-structures.yaspl"],
    deps = [
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:maybe",
    ],
)

yaspl_test(
    name = "database-test",
    srcs = ["database-test.yaspl"],
    deps = [
        ":relation-structures",
        "//libraries:yunit",
        "//libraries/data:bytes",
        "//libraries/data:dict",
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
    srcs = [
    ],
)
