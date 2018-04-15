load("//libraries/prim-language:prim-language.bzl", "prim_library")

prim_library(
    name = "fill-stack",
    srcs = ["fill-stack.prim"],
)

filegroup(
    name = "runtime",
    srcs = [":fill-stack"],
    visibility = ["//visibility:public"],
)
