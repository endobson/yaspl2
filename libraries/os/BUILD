load("@minimal_racket//:racket.bzl", "racket_binary", "racket_library", "racket_test")

package(
    default_visibility = ["//visibility:public"],
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

racket_library(
    name = "gpt",
    srcs = ["gpt.rkt"],
    deps = [
      ":util",
    ],
)

racket_library(
    name = "make-gpt-main",
    srcs = ["make-gpt-main.rkt"],
    deps = [
      ":util",
      ":fat32",
      ":gpt",
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
    name = "make-uefi-vars-main",
    srcs = ["make-uefi-vars-main.rkt"],
    deps = [
      ":util",
      ":fat32",
      ":gpt",
    ],
)

racket_binary(
    name = "make-uefi-vars",
    main_module = "make-uefi-vars-main.rkt",
    deps = [
        ":make-uefi-vars-main",
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


racket_library(
    name = "inflate",
    srcs = ["inflate.rkt"],
    deps = [
      ":util"
    ],
)

racket_library(
    name = "compress-disk-main",
    srcs = ["compress-disk-main.rkt"],
    deps = [
      ":util"
    ],
)


racket_binary(
    name = "compress-disk",
    main_module = "compress-disk-main.rkt",
    deps = [
        ":compress-disk-main",
    ],
)

racket_library(
    name = "compression-test",
    srcs = ["compression-test.rkt"],
    deps = [
      ":inflate"
    ],
)

racket_test(
    name = "compression-test_test",
    main_module = "compression-test.rkt",
    testonly = 0,
    deps = [
      ":compression-test",
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
