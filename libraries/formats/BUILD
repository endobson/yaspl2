load("//libraries:yaspl.bzl", "yaspl_bootstrap_library", "yaspl_library")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_bootstrap_library(
    name = "elf",
    srcs = ["elf.yaspl"],
    deps = [
        "//libraries:io",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
    ],
)

yaspl_bootstrap_library(
    name = "elf-writer",
    srcs = ["elf-writer.yaspl"],
    deps = [
        "//libraries:io",
        "//libraries:structured-elf",
        "//libraries:structured-native-code",
        "//libraries/data:aligned-bytes",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:indexed-set",
        "//libraries/data:join-list",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:set",
        "//libraries/data:tuples",
    ],
)

yaspl_library(
    name = "html",
    srcs = ["html.yaspl"],
    deps = [
        ":xml",
        "//libraries/data:join-list",
        "//libraries/data:list",
    ],
)

yaspl_bootstrap_library(
    name = "pe",
    srcs = ["pe.yaspl"],
    deps = [
        "//libraries:io",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:numbers",
    ],
)

yaspl_bootstrap_library(
    name = "pe-writer",
    srcs = ["pe-writer.yaspl"],
    deps = [
        "//libraries:io",
        "//libraries:structured-native-code",
        "//libraries:structured-pe",
        "//libraries/data:aligned-bytes",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:join-list",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/data:tuples",
    ],
)

yaspl_bootstrap_library(
    name = "sexp",
    srcs = ["sexp.yaspl"],
    deps = [
        "//libraries/data:list",
        "//libraries/data:source-location",
    ],
)

yaspl_library(
    name = "svg",
    srcs = ["svg.yaspl"],
    deps = [
        ":xml",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:numbers",
    ],
)

yaspl_library(
    name = "web-assembly",
    srcs = ["web-assembly.yaspl"],
    deps = [
        "//libraries/data:bytes",
        "//libraries/data:join-list",
        "//libraries/data:list",
    ],
)

yaspl_library(
    name = "xml",
    srcs = ["xml.yaspl"],
    deps = [
        "//libraries/data:bytes",
        "//libraries/data:join-list",
        "//libraries/data:list",
    ],
)
