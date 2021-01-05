load("//libraries/prim-language:prim-language.bzl", "prim_library")
load("//libraries/core-language:core-language.bzl", "core_library")
load("//libraries:yaspl.bzl", "yaspl_test")

package(
    default_visibility = ["//visibility:public"],
)

prim_library(
    name = "read-memory",
    srcs = ["read-memory.prim"],
)

prim_library(
    name = "variant-descriptor",
    srcs = ["variant-descriptor.prim"],
)

core_library(
    name = "fill-stack",
    srcs = ["fill-stack.core"],
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
        ":read-memory",
        ":variant-descriptor",
    ],
    visibility = ["//visibility:public"],
)

filegroup(
    name = "runtime-sources",
    srcs = [
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
