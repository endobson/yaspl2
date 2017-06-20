load("//libraries:yaspl.bzl", "yaspl_bootstrap_library", "yaspl_library")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_bootstrap_library(
    name = "x86-64-runtime",
    srcs = ["x86-64-runtime.yaspl"],
    deps = [
        "//libraries:x86-64-stack-machine",
        "//libraries/data:bytes",
        "//libraries/data:join-list",
        "//libraries/data:list",
    ],
)

yaspl_library(
    name = "intermediate-to-register",
    srcs = ["intermediate-to-register.yaspl"],
    deps = [
        "//libraries:intermediate-language",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
        "//libraries/prim-language:register-language",
        "//libraries/prim-language:register-language-builder",
    ],
)
