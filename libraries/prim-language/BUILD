load(
    "//libraries:yaspl.bzl",
    "yaspl_binary",
    "yaspl_bootstrap_binary",
    "yaspl_bootstrap_library",
    "yaspl_library",
    "yaspl_test",
)
load(":prim-language.bzl", "prim_language_toolchain")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_binary(
    name = "prim-language-compiler",
    srcs = ["prim-language-compiler-main.yaspl"],
    deps = [
        ":prim-language-compiler_lib",
        ":prim-language-runtime",
        "//libraries:io",
        "//libraries:machine-code-writer",
        "//libraries:system-abi",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/yaspl:x86-64-assembly",
    ],
)

yaspl_bootstrap_binary(
    name = "prim-language-library-compiler",
    srcs = ["prim-language-library-compiler-main.yaspl"],
    deps = [
        ":prim-language-compiler_lib",
        "//libraries:io",
        "//libraries:machine-code-writer",
        "//libraries:system-abi",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/yaspl:x86-64-assembly",
    ],
)

yaspl_bootstrap_library(
    name = "assigned-register-language",
    srcs = ["assigned-register-language.yaspl"],
    deps = [":lowered-register-language"],
)

yaspl_bootstrap_library(
    name = "block-number",
    srcs = ["block-number.yaspl"],
    deps = [
        "//libraries/data:numbers",
        "//libraries/data:ordering",
    ],
)

yaspl_bootstrap_library(
    name = "chained-register-language",
    srcs = ["chained-register-language.yaspl"],
    deps = [
        ":assigned-register-language",
        ":block-number",
        ":lowered-register-language",
        "//libraries/data:dict",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:set",
        "//libraries/data:tuples",
    ],
)

yaspl_bootstrap_library(
    name = "chained-register-language-to-x86-64",
    srcs = ["chained-register-language-to-x86-64.yaspl"],
    deps = [
        ":assigned-register-language",
        ":block-number",
        ":chained-register-language",
        ":lower-x86-64",
        "//libraries/data:bytes",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/yaspl:math-operations",
        "//libraries/yaspl:x86-64-assembly",
    ],
)

yaspl_bootstrap_library(
    name = "color-graph",
    srcs = ["color-graph.yaspl"],
    deps = [
        ":lower-x86-64",
        ":lowered-live-variables",
        "//libraries:panic",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/data:tuples",
        "//libraries/data:undirected-graph",
        "//libraries/yaspl:var",
    ],
)

yaspl_test(
    name = "color-graph-test",
    srcs = ["color-graph-test.yaspl"],
    deps = [
        ":color-graph",
        ":lower-x86-64",
        ":lowered-live-variables",
        ":prim-language",
        ":prim-to-register",
        "//libraries:system-abi",
        "//libraries:yunit",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
        "//libraries/yaspl:math-operations",
    ],
)

yaspl_bootstrap_library(
    name = "color-lowered-register-language",
    srcs = ["color-lowered-register-language.yaspl"],
    deps = [
        ":assigned-register-language",
        ":block-number",
        ":color-graph",
        ":lower-x86-64",
        ":lowered-live-variables",
        ":lowered-register-language",
        ":spill-lowered-register-language",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:ordering",
        "//libraries/data:tuples",
        "//libraries/yaspl:math-operations",
        "//libraries/yaspl:var",
    ],
)

yaspl_bootstrap_library(
    name = "lower-x86-64",
    srcs = ["lower-x86-64.yaspl"],
    deps = [
        ":block-number",
        ":lowered-register-language",
        ":register-language",
        ":register-language-used-vars",
        ":simplify-register-language",
        ":variant-info",
        "//libraries:panic",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:indexed-set",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:ordering",
        "//libraries/data:set",
        "//libraries/data:tuples",
        "//libraries/yaspl:math-operations",
        "//libraries/yaspl:var",
    ],
)

yaspl_test(
    name = "lower-x86-64-test",
    srcs = ["lower-x86-64-test.yaspl"],
    deps = [
        ":lower-x86-64",
        "//libraries:yunit",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
    ],
)

yaspl_bootstrap_library(
    name = "lowered-register-language",
    srcs = ["lowered-register-language.yaspl"],
    deps = [
        ":block-number",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set-multi-dict",
        "//libraries/data:tuples",
        "//libraries/yaspl:math-operations",
    ],
)

yaspl_bootstrap_library(
    name = "lowered-live-variables",
    srcs = ["lowered-live-variables.yaspl"],
    deps = [
        ":block-number",
        ":lowered-register-language",
        "//libraries:graphviz",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:ordering",
        "//libraries/data:set",
        "//libraries/data:tuples",
        "//libraries/data:undirected-graph",
    ],
)

yaspl_bootstrap_library(
    name = "prim-language",
    srcs = ["prim-language.yaspl"],
    deps = [
        ":syscalls",
        "//libraries/data:list",
        "//libraries/data:tuples",
        "//libraries/yaspl:math-operations",
    ],
)

yaspl_bootstrap_library(
    name = "prim-language-compiler_lib",
    srcs = ["prim-language-compiler.yaspl"],
    deps = [
        ":prim-language",
        ":prim-language-parser",
        ":prim-language-type-checker",
        ":prim-to-register",
        ":register-language-compiler",
        ":simplify-register-language",
        "//libraries:sexp-parser",
        "//libraries:system-abi",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
        "//libraries/yaspl:x86-64-assembly",
    ],
)

yaspl_bootstrap_library(
    name = "prim-language-parser",
    srcs = ["prim-language-parser.yaspl"],
    deps = [
        ":prim-language",
        ":syscalls",
        "//libraries:sexp-printer",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
        "//libraries/formats:sexp",
        "//libraries/yaspl:math-operations",
    ],
)

yaspl_bootstrap_library(
    name = "prim-language-runtime",
    srcs = ["prim-language-runtime.yaspl"],
    deps = [
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/yaspl:x86-64-assembly",
    ],
)

yaspl_bootstrap_library(
    name = "prim-language-type-checker",
    srcs = ["prim-language-type-checker.yaspl"],
    deps = [
        ":prim-language",
        ":syscalls",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
        "//libraries/yaspl:math-operations",
    ],
)

yaspl_library(
    name = "print-assigned-register-language",
    srcs = ["print-assigned-register-language.yaspl"],
    deps = [
        ":assigned-register-language",
        ":print-lowered-register-language",
        "//libraries/data:bytes",
        "//libraries/data:join-list",
    ],
)

yaspl_library(
    name = "print-lowered-register-language",
    srcs = ["print-lowered-register-language.yaspl"],
    deps = [
        ":block-number",
        ":lowered-register-language",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:numbers",
        "//libraries/yaspl:math-operations",
    ],
)

yaspl_library(
    name = "print-register-language",
    srcs = ["print-register-language.yaspl"],
    deps = [
        ":block-number",
        ":register-language",
        ":variant-info",
        "//libraries/data:bytes",
        "//libraries/data:indexed-set",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:numbers",
        "//libraries/yaspl:math-operations",
        "//libraries/yaspl:var",
    ],
)

yaspl_bootstrap_library(
    name = "prim-to-register",
    srcs = ["prim-to-register.yaspl"],
    deps = [
        ":block-number",
        ":prim-language",
        ":register-language",
        ":register-language-builder",
        ":syscalls",
        "//libraries:panic",
        "//libraries:system-abi",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:indexed-set",
        "//libraries/data:list",
        "//libraries/data:set",
        "//libraries/data:tuples",
        "//libraries/yaspl:math-operations",
        "//libraries/yaspl:var",
    ],
)

yaspl_bootstrap_library(
    name = "register-language",
    srcs = ["register-language.yaspl"],
    deps = [
        ":block-number",
        ":variant-info",
        "//libraries/data:indexed-set",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/yaspl:math-operations",
        "//libraries/yaspl:var",
    ],
)

yaspl_bootstrap_library(
    name = "register-language-builder",
    srcs = ["register-language-builder.yaspl"],
    deps = [
        ":block-number",
        ":register-language",
        "//libraries/data:indexed-set",
        "//libraries/data:list",
        "//libraries/data:tuples",
        "//libraries/yaspl:var",
    ],
)

yaspl_bootstrap_library(
    name = "register-language-compiler",
    srcs = ["register-language-compiler.yaspl"],
    deps = [
        ":chained-register-language",
        ":chained-register-language-to-x86-64",
        ":color-lowered-register-language",
        ":lower-x86-64",
        ":lowered-register-language",
        ":register-language",
        ":simplify-assigned-register-language",
        ":simplify-assigned-register-language-x86-64",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:tuples",
        "//libraries/yaspl:x86-64-assembly",
    ],
)

yaspl_bootstrap_library(
    name = "register-language-used-vars",
    srcs = ["register-language-used-vars.yaspl"],
    deps = [
        ":block-number",
        ":register-language",
        "//libraries/data:indexed-set",
        "//libraries/data:list",
        "//libraries/data:set",
        "//libraries/yaspl:var",
    ],
)

yaspl_bootstrap_library(
    name = "simplify-assigned-register-language",
    srcs = ["simplify-assigned-register-language.yaspl"],
    deps = [
        ":assigned-register-language",
        ":block-number",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:multi-set",
        "//libraries/data:set",
        "//libraries/yaspl:math-operations",
    ],
)

yaspl_bootstrap_library(
    name = "simplify-assigned-register-language-x86-64",
    srcs = ["simplify-assigned-register-language-x86-64.yaspl"],
    deps = [
        ":assigned-register-language",
        ":block-number",
        ":lower-x86-64",
        ":lowered-live-variables",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:set",
    ],
)

yaspl_bootstrap_library(
    name = "simplify-register-language",
    srcs = ["simplify-register-language.yaspl"],
    deps = [
        ":block-number",
        ":register-language",
        ":register-language-used-vars",
        ":variant-info",
        "//libraries:panic",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:indexed-set",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:set",
        "//libraries/data:tuples",
        "//libraries/yaspl:math-operations",
        "//libraries/yaspl:var",
    ],
)

yaspl_bootstrap_library(
    name = "spill-lowered-register-language",
    srcs = ["spill-lowered-register-language.yaspl"],
    deps = [
        ":block-number",
        ":lower-x86-64",
        ":lowered-live-variables",
        ":lowered-register-language",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:ordering",
        "//libraries/data:set",
        "//libraries/data:tuples",
        "//libraries/yaspl:var",
    ],
)

yaspl_bootstrap_library(
    name = "syscalls",
    srcs = ["syscalls.yaspl"],
    deps = [
        "//libraries:system-abi",
    ],
)

yaspl_bootstrap_library(
    name = "variant-info",
    srcs = ["variant-info.yaspl"],
    deps = [
    ],
)

toolchain_type(name = "prim_language_toolchain")

prim_language_toolchain(
    name = "osx_prim_language_toolchain_impl",
    platform = "osx",
)

toolchain(
    name = "osx_prim_language_toolchain",
    target_compatible_with = [
        "@platforms//os:osx",
        "@platforms//cpu:x86_64",
    ],
    toolchain = ":osx_prim_language_toolchain_impl",
    toolchain_type = ":prim_language_toolchain",
)

prim_language_toolchain(
    name = "linux_prim_language_toolchain_impl",
    platform = "linux",
)

toolchain(
    name = "linux_prim_language_toolchain",
    target_compatible_with = [
        "@platforms//os:linux",
        "@platforms//cpu:x86_64",
    ],
    toolchain = ":linux_prim_language_toolchain_impl",
    toolchain_type = ":prim_language_toolchain",
)

test_suite(
    name = "package_tests",
    testonly = 0,
)

filegroup(
    name = "package_binaries",
    srcs = [
        ":prim-language-compiler",
        ":prim-language-library-compiler",
    ],
)
