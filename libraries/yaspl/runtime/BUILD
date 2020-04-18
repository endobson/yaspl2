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
    name = "variant-descriptor",
    srcs = ["variant-descriptor.prim"],
)

yaspl_test(
    name = "fill-stack-test",
    size = "small",
    srcs = ["fill-stack-test.yaspl"],
    deps = [
        "//libraries:io",
        "//libraries:stack-trace",
        "//libraries:system-abi",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:numbers",
        "//libraries/data:tuples",
    ],
)

yaspl_test(
    name = "variant-descriptor-test",
    size = "small",
    srcs = ["variant-descriptor-test.yaspl"],
    deps = [
        "//libraries:yunit",
        "//libraries/data:bytes",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
    ],
)

filegroup(
    name = "runtime",
    srcs = [
        ":fill-stack",
        ":read-memory",
        ":variant-descriptor",
    ],
    visibility = ["//visibility:public"],
)

filegroup(
    name = "runtime-sources",
    srcs = [
        ":fill-stack.prim",
        ":read-memory.prim",
        ":variant-descriptor.prim",
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
