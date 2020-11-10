load("@minimal_racket//:racket.bzl", "racket_binary", "racket_library")

package(
    default_visibility = ["//visibility:public"],
)

racket_library(
    name = "make-gpt-main",
    srcs = ["make-gpt-main.rkt"],
    deps = [
    ],
)

racket_binary(
    name = "make-gpt",
    main_module = "make-gpt-main.rkt",
    deps = [
        ":make-gpt-main",
    ],
)

test_suite(
    name = "package_tests",
    testonly = 0,
)

filegroup(
    name = "package_binaries",
    srcs = [
    ],
)
