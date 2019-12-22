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
