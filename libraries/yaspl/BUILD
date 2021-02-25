load(
    "//libraries:yaspl.bzl",
    "yaspl_bootstrap_library",
    "yaspl_make_variables",
    "yaspl_test",
    "yaspl_toolchain",
)

package(
    default_visibility = ["//visibility:public"],
)

toolchain_type(name = "yaspl_toolchain")

yaspl_toolchain(
    name = "osx_yaspl_toolchain_impl",
    platform = "osx",
)

toolchain(
    name = "osx_yaspl_toolchain",
    target_compatible_with = [
        "@platforms//os:osx",
        "@platforms//cpu:x86_64",
    ],
    toolchain = ":osx_yaspl_toolchain_impl",
    toolchain_type = ":yaspl_toolchain",
)

yaspl_toolchain(
    name = "linux_yaspl_toolchain_impl",
    platform = "linux",
)

toolchain(
    name = "linux_yaspl_toolchain",
    target_compatible_with = [
        "@platforms//os:linux",
        "@platforms//cpu:x86_64",
    ],
    toolchain = ":linux_yaspl_toolchain_impl",
    toolchain_type = ":yaspl_toolchain",
)

yaspl_toolchain(
    name = "yasplos_yaspl_toolchain_impl",
    platform = "yasplos",
)

toolchain(
    name = "yasplos_yaspl_toolchain",
    target_compatible_with = [
        "//constraints/os:yasplos",
        "@platforms//cpu:x86_64",
    ],
    toolchain = ":yasplos_yaspl_toolchain_impl",
    toolchain_type = ":yaspl_toolchain",
)

yaspl_make_variables(
    name = "make_variables",
)

yaspl_bootstrap_library(
    name = "assembly-function",
    srcs = ["assembly-function.yaspl"],
    deps = [
        ":x86-64-assembly",
        "//libraries:prim-names",
        "//libraries/data:bytes",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:tuples",
    ],
)

yaspl_bootstrap_library(
    name = "intermediate-to-register",
    srcs = ["intermediate-to-register.yaspl"],
    deps = [
        ":mangle-names",
        ":math-operations",
        ":top-level-objects",
        ":var",
        "//libraries:intermediate-expression",
        "//libraries:intermediate-language",
        "//libraries:top-level-name",
        "//libraries:variant-info",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:indexed-set",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:tuples",
        "//libraries/prim-language:block-number",
        "//libraries/prim-language:register-language",
        "//libraries/prim-language:register-language-builder",
        "//libraries/prim-language:variant-info",
    ],
)

yaspl_bootstrap_library(
    name = "mangle-names",
    srcs = ["mangle-names.yaspl"],
    deps = [
        "//libraries:module-name",
        "//libraries:top-level-name",
        "//libraries/data:bytes",
        "//libraries/data:list",
        "//libraries/data:maybe",
    ],
)

yaspl_bootstrap_library(
    name = "math-operations",
    srcs = ["math-operations.yaspl"],
)

yaspl_bootstrap_library(
    name = "optimize-intermediate",
    srcs = ["optimize-intermediate.yaspl"],
    deps = [
        ":var",
        "//libraries:intermediate-expression",
        "//libraries:intermediate-language",
        "//libraries:module-signature",
        "//libraries:top-level-name",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
    ],
)

yaspl_bootstrap_library(
    name = "parser",
    srcs = ["parser.yaspl"],
    deps = [
        "//libraries:io",
        "//libraries:module-signature",
        "//libraries:module-signature-serialization",
        "//libraries:sexp-parser",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:source-location",
        "//libraries/yaspl/conversions:source-language-parser",
        "//libraries/yaspl/languages:source-language",
    ],
)

yaspl_bootstrap_library(
    name = "top-level-object-optimizer",
    srcs = ["top-level-object-optimizer.yaspl"],
    deps = [
        ":top-level-objects",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
        "//libraries/prim-language:simplify-register-language",
    ],
)

yaspl_bootstrap_library(
    name = "top-level-objects-to-x86-64",
    srcs = ["top-level-objects-to-x86-64.yaspl"],
    deps = [
        ":top-level-objects",
        ":x86-64-assembly",
        "//libraries:prim-names",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/prim-language:register-language-compiler",
    ],
)

yaspl_bootstrap_library(
    name = "top-level-objects",
    srcs = ["top-level-objects.yaspl"],
    deps = [
        "//libraries:module-signature",
        "//libraries/data:list",
        "//libraries/prim-language:register-language",
    ],
)

yaspl_bootstrap_library(
    name = "var",
    srcs = ["var.yaspl"],
    deps = [
        "//libraries/data:bytes",
        "//libraries/data:list",
        "//libraries/data:numbers",
        "//libraries/data:ordering",
    ],
)

yaspl_bootstrap_library(
    name = "x86-64-assembly",
    srcs = ["x86-64-assembly.yaspl"],
    deps = [
        "//libraries/data:bytes",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
    ],
)

yaspl_bootstrap_library(
    name = "x86-64-assembly-printer",
    srcs = ["x86-64-assembly-printer.yaspl"],
    deps = [
        ":x86-64-assembly",
        "//libraries/data:bytes",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:numbers",
        "//libraries/data:set",
    ],
)

yaspl_bootstrap_library(
    name = "x86-64-runtime",
    srcs = ["x86-64-runtime.yaspl"],
    deps = [
        ":assembly-function",
        ":mangle-names",
        ":x86-64-assembly",
        "//libraries:prim-names",
        "//libraries:system-abi",
        "//libraries:top-level-name",
        "//libraries/data:bytes",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:tuples",
    ],
)

yaspl_test(
    name = "x86-64-runtime-test",
    srcs = ["x86-64-runtime-test.yaspl"],
    deps = [
        ":x86-64-assembly",
        ":x86-64-runtime",
        "//libraries:system-abi",
        "//libraries:yunit",
        "//libraries/data:bytes",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
    ],
)

yaspl_bootstrap_library(
    name = "x86-64-instruction-assembler",
    srcs = ["x86-64-instruction-assembler.yaspl"],
    deps = [
        ":x86-64-assembly",
        "//libraries/data:bytes",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
    ],
)

test_suite(
    name = "package_tests",
    testonly = 0,
)

filegroup(
    name = "package_binaries",
    srcs = [],
)
