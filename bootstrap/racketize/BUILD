load("@minimal_racket//:racket.bzl", "racket_library")

package(
    default_visibility = ["//visibility:public"],
)

racket_library(
    name = "pattern",
    srcs = ["pattern.rkt"],
    deps = [
        "//bootstrap:machine-structs",
        "//bootstrap:parser-structs",
    ],
)

racket_library(
    name = "module",
    srcs = ["module.rkt"],
    deps = [
        ":pattern",
        "//bootstrap:machine-structs",
        "//bootstrap:parser-structs",
        "//bootstrap:signature-structs",
        "//bootstrap:utils",
    ],
)
