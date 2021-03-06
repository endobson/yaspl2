load("@minimal_racket//:racket.bzl", "racket_binary", "racket_library", "racket_test")
load("//libraries/core-language:core-language.bzl", "core_library")
load("//libraries:yaspl.bzl", "yaspl_binary", "yaspl_library", "yaspl_link")

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
        ":fat32",
        ":util",
    ],
)

racket_library(
    name = "make-gpt-main",
    srcs = ["make-gpt-main.rkt"],
    deps = [
        ":fat32",
        ":gpt",
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
    name = "make-uefi-vars-main",
    srcs = ["make-uefi-vars-main.rkt"],
    deps = [
        ":fat32",
        ":gpt",
        ":util",
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
        ":util",
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
        ":util",
    ],
)

racket_library(
    name = "compress-disk-main",
    srcs = ["compress-disk-main.rkt"],
    deps = [
        ":util",
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
        ":inflate",
    ],
)

racket_test(
    name = "compression-test_test",
    testonly = 0,
    main_module = "compression-test.rkt",
    deps = [
        ":compression-test",
    ],
)

# Actual OS Code

core_library(
    name = "examples",
    srcs = ["examples.core"],
    deps = [":serial"],
    signatures = [
      "msabi-runtime.sig",
    ],
)

core_library(
    name = "serial",
    srcs = ["serial.core"],
    signatures = ["msabi-runtime.sig"],
)


yaspl_library(
    name = "msabi",
    srcs = ["msabi.yaspl"],
    deps = [
        "//libraries:module-name",
        "//libraries:module-signature",
        "//libraries:prim-names",
        "//libraries:prim-types",
        "//libraries:top-level-name",
        "//libraries:types",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:tuples",
        "//libraries/yaspl:assembly-function",
        "//libraries/yaspl:x86-64-assembly",
    ],
)

yaspl_binary(
    name = "gen-msabi",
    srcs = ["gen-msabi-main.yaspl"],
    deps = [
        ":msabi",
        "//libraries:io",
        "//libraries:machine-code-writer",
        "//libraries:module-signature-serialization",
        "//libraries:system-abi",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
        "//libraries/yaspl:x86-64-assembly",
    ],
)

genrule(
    name = "gen-msabi-runtime",
    outs = [
        "msabi-runtime.o",
        "msabi-runtime.sig",
    ],
    cmd = "$(location :gen-msabi) $(YASPL_PLATFORM) " +
          "$(location :msabi-runtime.o) $(location :msabi-runtime.sig)",
    toolchains = ["//libraries/yaspl:make_variables"],
    tools = [":gen-msabi"],
)

yaspl_link(
    name = "kernel",
    objects = [
        ":msabi-runtime.o",
    ],
    output = "kernel.efi",
    deps = [
        ":examples",
    ],
)


test_suite(
    name = "package_tests",
    testonly = 0,
)

filegroup(
    name = "package_binaries",
    srcs = [
        "gen-msabi",
    ],
)
