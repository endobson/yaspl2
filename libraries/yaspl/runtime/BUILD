load("//libraries/prim-language:prim-language.bzl", "prim_library")
load("//libraries:yaspl.bzl", "yaspl_test")

prim_library(
    name = "fill-stack",
    srcs = ["fill-stack.prim"],
)

prim_library(
    name = "read-memory",
    srcs = ["read-memory.prim"],
)

prim_library(
    name = "type-descriptor",
    srcs = ["type-descriptor.prim"],
)

yaspl_test(
    name = "fill-stack-test",
    size = "small",
    srcs = ["fill-stack-test.yaspl"],
    tags = [
        "manual",
    ],
    deps = [
        "//libraries:io",
        "//libraries:stack-trace",
        "//libraries/data:bytes",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:tuples",
    ],
)

filegroup(
    name = "runtime",
    srcs = [
        ":fill-stack",
        ":read-memory",
        ":type-descriptor",
    ],
    visibility = ["//visibility:public"],
)

filegroup(
    name = "runtime-sources",
    srcs = [
        ":fill-stack.prim",
        ":read-memory.prim",
        ":type-descriptor.prim",
    ],
    visibility = [
        "//bootstrap:__pkg__",
    ],
)

test_suite(
    name = "package_tests",
    testonly = 0,
    visibility = ["//visibility:public"],
)

filegroup(
    name = "package_binaries",
    srcs = [],
    visibility = ["//visibility:public"],
)
