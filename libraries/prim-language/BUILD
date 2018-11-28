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
    name = "demo",
    srcs = ["prim-language-demo.yaspl"],
    deps = [
        ":color-graph",
        ":lower-x86-64",
        ":lowered-live-variables",
        ":lowered-register-language",
        ":lowered-register-language-to-x86-64",
        ":prim-language",
        ":prim-language-parser",
        ":prim-to-register",
        ":print-lowered-register-language",
        ":print-register-language",
        ":register-language",
        ":simplify-lowered-register-language",
        ":simplify-register-language",
        ":spill-lowered-register-language",
        "//libraries:io",
        "//libraries:sexp-parser",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/data:tuples",
        "//libraries/yaspl:var",
    ],
)

yaspl_binary(
    name = "prim-language-compiler",
    srcs = ["prim-language-compiler-main.yaspl"],
    deps = [
        ":prim-language-compiler_lib",
        ":prim-language-runtime",
        "//libraries:io",
        "//libraries:machine-code-writer",
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
        "//libraries:assembler",
        "//libraries:io",
        "//libraries:mach-o-writer",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:tuples",
    ],
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
        "//libraries:yunit",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/yaspl:math-operations",
    ],
)

yaspl_bootstrap_library(
    name = "lower-x86-64",
    srcs = ["lower-x86-64.yaspl"],
    deps = [
        ":block-number",
        ":lowered-register-language",
        ":prim-language",
        ":register-language",
        ":register-language-used-vars",
        ":simplify-register-language",
        ":syscalls",
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
        "//libraries/algorithms:find-chains",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/data:set-multi-dict",
        "//libraries/data:tuples",
        "//libraries/data:unique-dict",
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
    name = "lowered-register-language-to-x86-64",
    srcs = ["lowered-register-language-to-x86-64.yaspl"],
    deps = [
        ":block-number",
        ":lower-x86-64",
        ":lowered-register-language",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:ordering",
        "//libraries/yaspl:math-operations",
        "//libraries/yaspl:x86-64-assembly",
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
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
        "//libraries/yaspl:x86-64-assembly",
    ],
)

yaspl_bootstrap_library(
    name = "prim-language-parser",
    srcs = ["prim-language-parser.yaspl"],
    deps = [
        ":prim-language",
        ":syscalls",
        "//libraries:sexp-parser",
        "//libraries:sexp-printer",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
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
        ":prim-language",
        ":register-language",
        ":syscalls",
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
        "//libraries:panic",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:indexed-set",
        "//libraries/data:list",
        "//libraries/data:maybe",
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
        ":prim-language",
        ":syscalls",
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
        ":color-graph",
        ":lower-x86-64",
        ":lowered-live-variables",
        ":lowered-register-language",
        ":lowered-register-language-to-x86-64",
        ":register-language",
        ":simplify-lowered-register-language",
        ":simplify-lowered-register-language-x86-64",
        ":spill-lowered-register-language",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
        "//libraries/yaspl:var",
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
    name = "simplify-lowered-register-language",
    srcs = ["simplify-lowered-register-language.yaspl"],
    deps = [
        ":block-number",
        ":lowered-register-language",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/data:set-multi-dict",
        "//libraries/data:unique-dict",
        "//libraries/yaspl:math-operations",
    ],
)

yaspl_bootstrap_library(
    name = "simplify-lowered-register-language-x86-64",
    srcs = ["simplify-lowered-register-language-x86-64.yaspl"],
    deps = [
        ":block-number",
        ":lower-x86-64",
        ":lowered-live-variables",
        ":lowered-register-language",
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
        ":prim-language",
        ":register-language",
        ":register-language-used-vars",
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
        "@bazel_tools//platforms:osx",
        "@bazel_tools//platforms:x86_64",
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
        "@bazel_tools//platforms:linux",
        "@bazel_tools//platforms:x86_64",
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
        ":demo",
        ":prim-language-compiler",
        ":prim-language-library-compiler",
    ],
)
