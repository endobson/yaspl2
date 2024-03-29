load(
    "//libraries:yaspl.bzl",
    "yaspl_bootstrap_library",
)

package(
    default_visibility = ["//visibility:public"],
)

yaspl_bootstrap_library(
    name = "lift-values",
    srcs = ["lift-values.yaspl"],
    deps = [
        "//libraries:module-name",
        "//libraries:top-level-name",
        "//libraries/core-language/languages:lifted-values-language",
        "//libraries/core-language/languages:resolved-vars-language",
        "//libraries/data:bytes",
        "//libraries/data:list",
        "//libraries/data:numbers",
        "//libraries/data:tuples",
    ],
)

yaspl_bootstrap_library(
    name = "lower-to-register",
    srcs = ["lower-to-register.yaspl"],
    deps = [
        "//libraries:module-signature",
        "//libraries:top-level-name",
        "//libraries:types",
        "//libraries/core-language:sized-types",
        "//libraries/core-language:struct-info",
        "//libraries/core-language/languages:lifted-values-language",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:indexed-set",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/data:tuples",
        "//libraries/prim-language:block-number",
        "//libraries/prim-language:register-language",
        "//libraries/prim-language:register-language-builder",
        "//libraries/yaspl:mangle-names",
        "//libraries/yaspl:math-operations",
        "//libraries/yaspl:top-level-objects",
        "//libraries/yaspl:var",
    ],
)

yaspl_bootstrap_library(
    name = "process-structs",
    srcs = ["process-structs.yaspl"],
    deps = [
        "//libraries:prim-types",
        "//libraries:types",
        "//libraries/core-language:sized-types",
        "//libraries/core-language:struct-info",
        "//libraries/core-language/languages:processed-structs-language",
        "//libraries/core-language/languages:resolved-types-language",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
    ],
)

yaspl_bootstrap_library(
    name = "resolve-imports",
    srcs = ["resolve-imports.yaspl"],
    deps = [
        "//libraries:module-name",
        "//libraries:module-signature",
        "//libraries/core-language/languages:resolved-imports-language",
        "//libraries/core-language/languages:source-language",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:indexed-set",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
    ],
)

yaspl_bootstrap_library(
    name = "resolve-types",
    srcs = ["resolve-types.yaspl"],
    deps = [
        "//libraries:module-signature",
        "//libraries:prim-types",
        "//libraries:types",
        "//libraries/core-language:struct-info",
        "//libraries/core-language/languages:resolved-imports-language",
        "//libraries/core-language/languages:resolved-types-language",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
    ],
)

yaspl_bootstrap_library(
    name = "resolve-vars",
    srcs = ["resolve-vars.yaspl"],
    deps = [
        "//libraries:module-name",
        "//libraries:module-signature",
        "//libraries:top-level-name",
        "//libraries:types",
        "//libraries/core-language/languages:resolved-vars-language",
        "//libraries/core-language/languages:type-checked-language",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
        "//libraries/yaspl:var",
    ],
)

yaspl_bootstrap_library(
    name = "type-check",
    srcs = ["type-check.yaspl"],
    deps = [
        "//libraries:module-signature",
        "//libraries:prim-types",
        "//libraries:sexp-printer",
        "//libraries:types",
        "//libraries/core-language:sized-types",
        "//libraries/core-language:struct-info",
        "//libraries/core-language/languages:processed-structs-language",
        "//libraries/core-language/languages:type-checked-language",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/data:tuples",
        "//libraries/yaspl:math-operations",
    ],
)
