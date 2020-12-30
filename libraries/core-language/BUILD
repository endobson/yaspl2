load(
    "//libraries:yaspl.bzl",
    "yaspl_binary",
    "yaspl_library",
    "yaspl_test",
)
load(":core-language.bzl", "core_language_toolchain")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_binary(
    name = "compiler",
    srcs = ["compiler.yaspl"],
    deps = [
        ":core-parser",
        ":lexer",
        ":lr-parser",
        ":source-to-register",
        "//libraries:io",
        "//libraries:machine-code-writer",
        "//libraries:module-signature-serialization",
        "//libraries:system-abi",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/yaspl:top-level-object-optimizer",
        "//libraries/yaspl:top-level-objects-to-x86-64",
    ],
)

yaspl_library(
    name = "core-parser",
    srcs = ["core-parser.yaspl"],
    deps = [
        ":lexer",
        ":lr-parser",
        ":source-language",
        "//libraries:module-name",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:numbers",
        "//libraries/data:ordering",
        "//libraries/data:tuples",
    ],
)

yaspl_library(
    name = "lexer",
    srcs = ["lexer.yaspl"],
    deps = [
        "//libraries:byte-escapes",
        "//libraries:lexer",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:numbers",
        "//libraries/data:ordering",
        "//libraries/data:source-location",
    ],
)

yaspl_test(
    name = "lexer-test",
    size = "small",
    srcs = ["lexer-test.yaspl"],
    deps = [
        ":lexer",
        "//libraries:yunit",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
    ],
)

yaspl_library(
    name = "lr-parser",
    srcs = ["lr-parser.yaspl"],
    deps = [
        ":lexer",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:ordering",
        "//libraries/data:set",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
        "//libraries/data:unique-dict",
    ],
)

yaspl_test(
    name = "lr-parser-test",
    size = "small",
    srcs = ["lr-parser-test.yaspl"],
    deps = [
        ":lexer",
        ":lr-parser",
        "//libraries:yunit",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:ordering",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
    ],
)

yaspl_library(
    name = "source-language",
    srcs = ["source-language.yaspl"],
    deps = [
        "//libraries:module-name",
        "//libraries/data:list",
    ],
)

yaspl_library(
    name = "source-to-register",
    srcs = ["source-to-register.yaspl"],
    deps = [
        ":source-language",
        "//libraries:module-signature",
        "//libraries:prim-types",
        "//libraries:top-level-name",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:tuples",
        "//libraries/yaspl:intermediate-to-register",
        "//libraries/yaspl:top-level-objects",
    ],
)

toolchain_type(name = "core_language_toolchain")

core_language_toolchain(
    name = "osx_core_language_toolchain_impl",
    platform = "osx",
)

toolchain(
    name = "osx_core_language_toolchain",
    target_compatible_with = [
        "@platforms//os:osx",
        "@platforms//cpu:x86_64",
    ],
    toolchain = ":osx_core_language_toolchain_impl",
    toolchain_type = ":core_language_toolchain",
)

core_language_toolchain(
    name = "linux_core_language_toolchain_impl",
    platform = "linux",
)

toolchain(
    name = "linux_core_language_toolchain",
    target_compatible_with = [
        "@platforms//os:linux",
        "@platforms//cpu:x86_64",
    ],
    toolchain = ":linux_core_language_toolchain_impl",
    toolchain_type = ":core_language_toolchain",
)


filegroup(
    name = "package_binaries",
    srcs = [
        ":compiler",
    ],
)

test_suite(
    name = "package_tests",
    testonly = 0,
)
