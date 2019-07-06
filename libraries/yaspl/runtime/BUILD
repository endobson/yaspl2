load("//libraries/prim-language:prim-language.bzl", "prim_library")

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
        ":type-descriptor",
    ],
    visibility = [
        "//bootstrap:__pkg__",
    ],
)
