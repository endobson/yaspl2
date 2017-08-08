load("//libraries:yaspl.bzl", "yaspl_bootstrap_library")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_bootstrap_library(
    name = "intermediate-to-register",
    srcs = ["intermediate-to-register.yaspl"],
    deps = [
        ":math-operations",
        ":module-environment",
        ":top-level-objects",
        "//libraries:intermediate-language",
        "//libraries:intermediate-to-stack",
        "//libraries:module-name",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:tuples",
        "//libraries/prim-language",
        "//libraries/prim-language:register-language",
        "//libraries/prim-language:register-language-builder",
    ],
)

yaspl_bootstrap_library(
    name = "math-operations",
    srcs = ["math-operations.yaspl"],
)

yaspl_bootstrap_library(
    name = "module-environment",
    srcs = ["module-environment.yaspl"],
    deps = [
        "//libraries:intermediate-language",
        "//libraries:module-name",
        "//libraries:module-signature",
        "//libraries:resolved-imports-language",
        "//libraries/data:bytes",
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
        "//libraries:stack-machine-optimizer",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/prim-language:simplify-register-language",
    ],
)

yaspl_bootstrap_library(
    name = "top-level-objects-to-x86-64",
    srcs = ["top-level-objects-to-x86-64.yaspl"],
    deps = [
        ":top-level-objects",
        ":x86-64-assembly",
        "//libraries:x86-64-stack-machine",
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
        "//libraries:stack-machine",
        "//libraries/data:list",
        "//libraries/prim-language:register-language",
    ],
)

yaspl_bootstrap_library(
    name = "x86-64-assembly",
    srcs = ["x86-64-assembly.yaspl"],
    deps = [
        "//libraries/data:bytes",
        "//libraries/data:join-list",
        "//libraries/data:list",
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
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:set",
    ],
)

yaspl_bootstrap_library(
    name = "x86-64-runtime",
    srcs = ["x86-64-runtime.yaspl"],
    deps = [
        ":x86-64-assembly",
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
    ],
)
