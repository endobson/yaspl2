load("//libraries:yaspl.bzl", "yaspl_bootstrap_library", "yaspl_toolchain")

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
        "@bazel_tools//platforms:osx",
        "@bazel_tools//platforms:x86_64",
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
        "@bazel_tools//platforms:linux",
        "@bazel_tools//platforms:x86_64",
    ],
    toolchain = ":linux_yaspl_toolchain_impl",
    toolchain_type = ":yaspl_toolchain",
)



yaspl_bootstrap_library(
    name = "intermediate-to-register",
    srcs = ["intermediate-to-register.yaspl"],
    deps = [
        ":math-operations",
        ":top-level-objects",
        ":var",
        "//libraries:intermediate-expression",
        "//libraries:intermediate-language",
        "//libraries:module-name",
        "//libraries:top-level-name",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:indexed-set",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
        "//libraries/prim-language",
        "//libraries/prim-language:block-number",
        "//libraries/prim-language:register-language",
        "//libraries/prim-language:register-language-builder",
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
        "//libraries:intermediate-expression",
        "//libraries:intermediate-language",
        "//libraries:module-signature",
        "//libraries:top-level-name",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:maybe",
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
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:tuples",
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
        "//libraries:io",
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
        ":intermediate-to-register",
        ":x86-64-assembly",
        "//libraries:machine-code",
        "//libraries:top-level-name",
        "//libraries/data:bytes",
        "//libraries/data:join-list",
        "//libraries/data:list",
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
