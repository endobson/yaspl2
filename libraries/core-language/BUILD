load(
    "//libraries:yaspl.bzl",
    "yaspl_binary",
    "yaspl_bootstrap_binary",
    "yaspl_bootstrap_library",
    "yaspl_test",
)
load(":core-language.bzl", "core_language_toolchain")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_bootstrap_binary(
    name = "compiler",
    srcs = ["compiler-main.yaspl"],
    deps = [
        ":compiler-lib",
        "//libraries:io",
        "//libraries:machine-code-writer",
        "//libraries:system-abi",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
    ],
)

yaspl_bootstrap_library(
    name = "compiler-lib",
    srcs = ["compiler.yaspl"],
    deps = [
        ":core-parser",
        ":lexer",
        ":lr-parser",
        ":source-to-register",
        "//libraries:io",
        "//libraries:module-signature",
        "//libraries:prim-implementation",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/yaspl:parser",
        "//libraries/yaspl:top-level-objects",
    ],
)

yaspl_binary(
    name = "core-grammar-graph",
    srcs = ["core-grammar-graph.yaspl"],
    deps = [
        ":core-parser",
        ":lr-parser",
        "//libraries:graphviz",
        "//libraries:io",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:directed-graph",
        "//libraries/data:either",
        "//libraries/data:join-list",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:set",
    ],
)

yaspl_bootstrap_library(
    name = "core-parser",
    srcs = ["core-parser.yaspl"],
    deps = [
        ":lexer",
        ":lr-parser",
        "//libraries:module-name",
        "//libraries/core-language/languages:source-language",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:ordering",
        "//libraries/data:tuples",
        "//libraries/yaspl:math-operations",
    ],
)

yaspl_test(
    name = "compiler-test",
    srcs = ["compiler-test.yaspl"],
    deps = [
        ":core-parser",
        ":lexer",
        ":lr-parser",
        ":source-to-register",
        "//libraries:prim-implementation",
        "//libraries:yunit",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
    ],
)

yaspl_bootstrap_library(
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

yaspl_bootstrap_library(
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
        "//libraries/data:set",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
    ],
)

yaspl_bootstrap_library(
    name = "sized-types",
    srcs = ["sized-types.yaspl"],
    deps = [
        "//libraries:types",
        "//libraries/data:lifted-primitives",
        "//libraries/data:maybe",
    ],
)

yaspl_bootstrap_library(
    name = "source-to-register",
    srcs = ["source-to-register.yaspl"],
    deps = [
        "//libraries:module-signature",
        "//libraries/core-language/conversions:lift-values",
        "//libraries/core-language/conversions:lower-to-register",
        "//libraries/core-language/conversions:process-structs",
        "//libraries/core-language/conversions:resolve-imports",
        "//libraries/core-language/conversions:resolve-types",
        "//libraries/core-language/conversions:resolve-vars",
        "//libraries/core-language/conversions:type-check",
        "//libraries/core-language/languages:source-language",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/yaspl:top-level-object-optimizer",
        "//libraries/yaspl:top-level-objects",
    ],
)

yaspl_bootstrap_library(
    name = "struct-info",
    srcs = ["struct-info.yaspl"],
    deps = [
        "//libraries:types",
        "//libraries/data:dict",
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

core_language_toolchain(
    name = "yasplos_core_language_toolchain_impl",
    platform = "yasplos",
)

toolchain(
    name = "yasplos_core_language_toolchain",
    target_compatible_with = [
        "//constraints/os:yasplos",
        "@platforms//cpu:x86_64",
    ],
    toolchain = ":yasplos_core_language_toolchain_impl",
    toolchain_type = ":core_language_toolchain",
)

filegroup(
    name = "package_binaries",
    srcs = [
        ":compiler",
        ":core-grammar-graph",
    ],
)

test_suite(
    name = "package_tests",
    testonly = 0,
)
