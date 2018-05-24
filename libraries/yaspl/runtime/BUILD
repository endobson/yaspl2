load("//libraries/prim-language:prim-language.bzl", "prim_library")

prim_library(
    name = "fill-stack",
    srcs = ["fill-stack.prim"],
)

prim_library(
    name = "read-memory",
    srcs = ["read-memory.prim"],
)

filegroup(
    name = "runtime",
    srcs = [
        ":fill-stack",
        ":read-memory",
    ],
    visibility = ["//visibility:public"],
)

filegroup(
    name = "runtime-sources",
    srcs = [
        ":fill-stack.prim",
        ":read-memory.prim",
    ],
    visibility = [
        "//bootstrap:__pkg__",
    ],
)
