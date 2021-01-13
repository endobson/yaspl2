load(
    "//libraries:yaspl.bzl",
    "yaspl_library",
)

package(
    default_visibility = ["//visibility:public"],
)

yaspl_library(
    name = "lower-to-register",
    srcs = ["lower-to-register.yaspl"],
    deps = [
        "//libraries:module-signature",
        "//libraries:prim-types",
        "//libraries:top-level-name",
        "//libraries:types",
        "//libraries/core-language/languages:resolved-vars-language",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:indexed-set",
        "//libraries/data:list",
        "//libraries/data:set",
        "//libraries/data:tuples",
        "//libraries/prim-language:block-number",
        "//libraries/prim-language:register-language",
        "//libraries/prim-language:register-language-builder",
        "//libraries/yaspl:intermediate-to-register",
        "//libraries/yaspl:math-operations",
        "//libraries/yaspl:top-level-objects",
        "//libraries/yaspl:var",
    ],
)

yaspl_library(
    name = "resolve-imports",
    srcs = ["resolve-imports.yaspl"],
    deps = [
        "//libraries/core-language:source-language",
        "//libraries/core-language/languages:resolved-imports-language",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
    ],
)

yaspl_library(
    name = "resolve-vars",
    srcs = ["resolve-vars.yaspl"],
    deps = [
        "//libraries:module-name",
        "//libraries:top-level-name",
        "//libraries/core-language/languages:resolved-imports-language",
        "//libraries/core-language/languages:resolved-vars-language",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
        "//libraries/yaspl:var",
    ],
)
