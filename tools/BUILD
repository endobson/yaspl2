load("//libraries:yaspl.bzl", "yaspl_binary", "yaspl_library")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_library(
    name = "analysis",
    srcs = ["analysis.yaspl"],
    deps = [
        "//libraries:io",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/yaspl:parser",
        "//libraries/yaspl/languages:source-language",
    ],
)

yaspl_binary(
    name = "aspect-clean-up-imports",
    srcs = ["aspect-clean-up-imports-main.yaspl"],
    deps = [
        ":command-line",
        ":linter_lib",
        ":rewrite-imports",
        "//libraries:io",
        "//libraries:module-name",
        "//libraries:unused-bindings",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/yaspl:parser",
        "//libraries/yaspl/languages:source-language",
    ],
)

yaspl_binary(
    name = "aspect-analysis",
    srcs = ["aspect-analysis-main.yaspl"],
    deps = [
        ":analysis",
        ":command-line",
        "//libraries/data:either",
        "//libraries/data:list",
    ],
)

yaspl_binary(
    name = "aspect-exported-values",
    srcs = ["aspect-exported-values-main.yaspl"],
    deps = [
        ":command-line",
        "//libraries:io",
        "//libraries:module-name",
        "//libraries:sexp-printer",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/yaspl:parser",
        "//libraries/yaspl/languages:source-language",
    ],
)

yaspl_binary(
    name = "aspect-ir-validator",
    srcs = ["aspect-ir-validator-main.yaspl"],
    deps = [
        ":command-line",
        ":compile-module-args",
        "//libraries:compiler_lib",
        "//libraries:io",
        "//libraries:module-name",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:indexed-set",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/prim-language:block-number",
        "//libraries/prim-language:register-language",
        "//libraries/prim-language:simplify-register-language",
        "//libraries/yaspl:top-level-objects",
        "//libraries/yaspl/frontend:errors",
        "//libraries/yaspl/languages:source-language",
    ],
)

yaspl_binary(
    name = "aspect-linter",
    srcs = ["aspect-linter-main.yaspl"],
    deps = [
        ":command-line",
        ":linter_lib",
        "//libraries/data:either",
        "//libraries/data:list",
    ],
)

yaspl_binary(
    name = "aspect-missing-dependencies",
    srcs = ["aspect-missing-dependencies-main.yaspl"],
    deps = [
        ":command-line",
        ":module-index-parser",
        "//libraries:io",
        "//libraries:module-name",
        "//libraries:prim-types",
        "//libraries:sexp-parser",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/yaspl:parser",
        "//libraries/yaspl/languages:source-language",
    ],
)

yaspl_binary(
    name = "aspect-module-dependency-graph",
    srcs = ["aspect-module-dependency-graph-main.yaspl"],
    deps = [
        ":command-line",
        "//libraries:io",
        "//libraries:module-name",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:tuples",
        "//libraries/yaspl:parser",
        "//libraries/yaspl/languages:source-language",
    ],
)

yaspl_binary(
    name = "aspect-remove-unused-dependencies",
    srcs = ["aspect-remove-unused-dependencies-main.yaspl"],
    deps = [
        ":command-line",
        ":module-index-parser",
        "//libraries:io",
        "//libraries:module-name",
        "//libraries:sexp-parser",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/yaspl:parser",
        "//libraries/yaspl/languages:source-language",
    ],
)

yaspl_library(
    name = "command-line",
    srcs = ["command-line.yaspl"],
    deps = [
        "//libraries:io",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
    ],
)

yaspl_library(
    name = "compile-module-args",
    srcs = ["compile-module-args.yaspl"],
    deps = [
        "//libraries:module-signature",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/yaspl:parser",
        "//libraries/yaspl/languages:source-language",
    ],
)

yaspl_binary(
    name = "intermediate-html-printer",
    srcs = ["intermediate-html-printer-main.yaspl"],
    deps = [
        ":command-line",
        ":compile-module-args",
        "//libraries:io",
        "//libraries:module-name",
        "//libraries:module-signature",
        "//libraries:prim-implementation",
        "//libraries:source-to-intermediate-language",
        "//libraries:validator",
        "//libraries/data:either",
        "//libraries/data:indexed-set",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/formats:xml",
        "//libraries/yaspl/conversions:resolve-imports",
        "//libraries/yaspl/conversions:resolve-types",
        "//libraries/yaspl/debug:intermediate-to-html",
    ],
)

yaspl_binary(
    name = "ir-printer",
    srcs = ["ir-printer-main.yaspl"],
    deps = [
        ":command-line",
        ":compile-module-args",
        "//libraries:compiler_lib",
        "//libraries:io",
        "//libraries:panic",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:join-list",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/prim-language:color-graph",
        "//libraries/prim-language:lower-x86-64",
        "//libraries/prim-language:lowered-live-variables",
        "//libraries/prim-language:print-lowered-register-language",
        "//libraries/prim-language:print-register-language",
        "//libraries/prim-language:register-language-compiler",
        "//libraries/yaspl:top-level-objects",
        "//libraries/yaspl:var",
        "//libraries/yaspl:x86-64-assembly-printer",
        "//libraries/yaspl/frontend:errors",
    ],
)

yaspl_library(
    name = "linter_lib",
    srcs = ["linter.yaspl"],
    deps = [
        "//libraries:io",
        "//libraries:lexer",
        "//libraries:module-header-lint",
        "//libraries:module-name",
        "//libraries:module-signature",
        "//libraries:unused-bindings",
        "//libraries:unused-local-bindings",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:indexed-set",
        "//libraries/data:list",
        "//libraries/data:set",
        "//libraries/data:source-location",
        "//libraries/yaspl:parser",
        "//libraries/yaspl/languages:source-language",
    ],
)

yaspl_library(
    name = "rewrite-imports",
    srcs = ["rewrite-imports.yaspl"],
    deps = [
        "//libraries:io",
        "//libraries:lexer",
        "//libraries:module-name",
        "//libraries:sexp-lexer",
        "//libraries:sexp-parser",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:source-location",
        "//libraries/yaspl/languages:source-language",
    ],
)

yaspl_binary(
    name = "module-index-validator",
    srcs = ["module-index-validator-main.yaspl"],
    deps = [
        ":command-line",
        ":module-index-parser",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
    ],
)

yaspl_library(
    name = "module-index-parser",
    srcs = ["module-index-parser.yaspl"],
    deps = [
        "//libraries:io",
        "//libraries:module-name",
        "//libraries:sexp-parser",
        "//libraries:sexp-printer",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
        "//libraries/formats:sexp",
    ],
)

test_suite(
    name = "package_tests",
    testonly = 0,
)

filegroup(
    name = "package_binaries",
    srcs = [
        ":aspect-analysis",
        ":aspect-clean-up-imports",
        ":aspect-exported-values",
        ":aspect-ir-validator",
        ":aspect-linter",
        ":aspect-missing-dependencies",
        ":aspect-module-dependency-graph",
        ":aspect-remove-unused-dependencies",
        ":intermediate-html-printer",
        ":ir-printer",
        ":module-index-validator",
        ":rewrite-imports",
    ],
)
