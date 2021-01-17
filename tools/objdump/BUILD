load("//libraries:yaspl.bzl", "yaspl_binary", "yaspl_library")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_binary(
    name = "objdump",
    srcs = ["objdump.yaspl"],
    deps = [
        "//libraries:io",
        "//libraries:machine-code-writer",
        "//libraries:system-abi",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/formats:pe",
        "//libraries/prim-language:prim-language-compiler_lib",
        "//libraries/prim-language:prim-language-runtime",
        "//libraries/yaspl:x86-64-assembly",
    ],
)

test_suite(
    name = "package_tests",
    testonly = 0,
)

filegroup(
    name = "package_binaries",
    srcs = [
        ":objdump",
    ],
)
