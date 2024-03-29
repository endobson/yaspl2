load("//libraries/prim-language:prim-language.bzl", "prim_library")
load("//libraries/core-language:core-language.bzl", "core_library")
load("//libraries:yaspl.bzl", "yaspl_test")
load(":filegroup_list.bzl", "filegroup_list")
load(":runtime.bzl", "runtime_objects")

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

core_library(
    name = "initialize-heap",
    srcs = ["initialize-heap.core"],
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

runtime_objects(
    name = "runtime",
    deps = [
        ":initialize-heap",
        ":read-memory",
        ":variant-descriptor",
    ],
)

filegroup(
    name = "runtime-sources",
    srcs = [
        ":initialize-heap.core",
        ":read-memory.prim",
        ":variant-descriptor.prim",
    ],
    visibility = [
        "//bootstrap:__pkg__",
    ],
)

filegroup_list(
    name = "runtime-sources.list",
    dep = "runtime-sources",
)

test_suite(
    name = "package_tests",
    testonly = 0,
)

filegroup(
    name = "package_binaries",
    srcs = [],
)
