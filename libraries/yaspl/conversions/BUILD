load(
    "//libraries:yaspl.bzl",
    "yaspl_bootstrap_library",
    "yaspl_test",
)

package(
    default_visibility = ["//visibility:public"],
)

yaspl_bootstrap_library(
    name = "expand-datatypes",
    srcs = ["expand-datatypes.yaspl"],
    deps = [
        "//libraries:module-name",
        "//libraries:module-signature",
        "//libraries:types",
        "//libraries:variant-info",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
        "//libraries/yaspl/languages:expanded-datatypes-language",
        "//libraries/yaspl/languages:resolved-types-language",
    ],
)

yaspl_bootstrap_library(
    name = "expand-patterns",
    srcs = ["expand-patterns.yaspl"],
    deps = [
        "//libraries:panic",
        "//libraries:variant-info",
        "//libraries/data:dict",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/data:tuples",
        "//libraries/yaspl:var",
        "//libraries/yaspl/languages:expanded-patterns-language",
        "//libraries/yaspl/languages:expanded-varargs-language",
    ],
)

yaspl_bootstrap_library(
    name = "expand-varargs",
    srcs = ["expand-varargs.yaspl"],
    deps = [
        "//libraries/data:list",
        "//libraries/yaspl/languages:expanded-varargs-language",
        "//libraries/yaspl/languages:resolved-vars-language",
    ],
)

yaspl_bootstrap_library(
    name = "resolve-imports",
    srcs = ["resolve-imports.yaspl"],
    deps = [
        "//libraries:module-name",
        "//libraries:module-signature",
        "//libraries:top-level-name",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:indexed-set",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
        "//libraries/yaspl/languages:resolved-imports-language",
        "//libraries/yaspl/languages:source-language",
    ],
)

yaspl_test(
    name = "resolve-imports-test",
    size = "small",
    srcs = ["resolve-imports-test.yaspl"],
    deps = [
        ":resolve-imports",
        "//libraries:module-name",
        "//libraries:module-signature",
        "//libraries:top-level-name",
        "//libraries:variant-info",
        "//libraries:yunit",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:indexed-set",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
        "//libraries/yaspl/languages:resolved-imports-language",
        "//libraries/yaspl/languages:source-language",
    ],
)

yaspl_bootstrap_library(
    name = "resolve-types",
    srcs = ["resolve-types.yaspl"],
    deps = [
        "//libraries:module-name",
        "//libraries:module-signature",
        "//libraries:types",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
        "//libraries/yaspl/languages:resolved-imports-language",
        "//libraries/yaspl/languages:resolved-types-language",
    ],
)

yaspl_bootstrap_library(
    name = "resolve-vars",
    srcs = ["resolve-vars.yaspl"],
    deps = [
        "//libraries:module-name",
        "//libraries:module-signature",
        "//libraries:top-level-name",
        "//libraries:type-checked-language",
        "//libraries:types",
        "//libraries:variant-info",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
        "//libraries/yaspl:var",
        "//libraries/yaspl/languages:resolved-vars-language",
    ],
)

yaspl_bootstrap_library(
    name = "source-language-parser",
    srcs = ["source-language-parser.yaspl"],
    deps = [
        "//libraries:module-name",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
        "//libraries/formats:sexp",
        "//libraries/yaspl/languages:source-language",
    ],
)

yaspl_test(
    name = "source-language-parser-test",
    size = "small",
    srcs = ["source-language-parser-test.yaspl"],
    deps = [
        ":source-language-parser",
        "//libraries:sexp-parser",
        "//libraries:yunit",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
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
