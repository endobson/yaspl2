load("@minimal_racket//:racket.bzl", "racket_binary", "racket_library")

package(
    default_visibility = ["//visibility:public"],
)

racket_library(
    name = "make-gpt-main",
    srcs = ["make-gpt-main.rkt"],
    deps = [
      ":util",
      ":fat32"
    ],
)

racket_library(
    name = "util",
    srcs = ["util.rkt"],
    deps = [
    ],
)

racket_library(
    name = "fat32",
    srcs = ["fat32.rkt"],
    deps = [
      ":util",
    ],
)

racket_binary(
    name = "make-gpt",
    main_module = "make-gpt-main.rkt",
    deps = [
        ":make-gpt-main",
    ],
)


racket_library(
    name = "make-exe-main",
    srcs = ["make-exe-main.rkt"],
    deps = [
      ":util"
    ],
)

racket_binary(
    name = "make-exe",
    main_module = "make-exe-main.rkt",
    deps = [
        ":make-exe-main",
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
