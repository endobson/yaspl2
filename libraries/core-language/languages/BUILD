load(
    "//libraries:yaspl.bzl",
    "yaspl_bootstrap_library",
)

package(
    default_visibility = ["//visibility:public"],
)

yaspl_bootstrap_library(
    name = "processed-structs-language",
    srcs = ["processed-structs-language.yaspl"],
    deps = [
        ":resolved-types-language",
        "//libraries:module-name",
        "//libraries:types",
        "//libraries/core-language:struct-info",
        "//libraries/data:dict",
        "//libraries/data:list",
    ],
)

yaspl_bootstrap_library(
    name = "lifted-values-language",
    srcs = ["lifted-values-language.yaspl"],
    deps = [
        ":resolved-vars-language",
        "//libraries:module-name",
        "//libraries:module-signature",
        "//libraries:top-level-name",
        "//libraries:types",
        "//libraries/core-language:sized-types",
        "//libraries/core-language:struct-info",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/yaspl:math-operations",
        "//libraries/yaspl:var",
    ],
)

yaspl_bootstrap_library(
    name = "resolved-imports-language",
    srcs = ["resolved-imports-language.yaspl"],
    deps = [
        ":source-language",
        "//libraries:module-name",
        "//libraries:module-signature",
        "//libraries/data:dict",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
    ],
)

yaspl_bootstrap_library(
    name = "resolved-types-language",
    srcs = ["resolved-types-language.yaspl"],
    deps = [
        "//libraries:module-name",
        "//libraries:top-level-name",
        "//libraries:types",
        "//libraries/core-language:struct-info",
        "//libraries/data:dict",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/yaspl:math-operations",
    ],
)

yaspl_bootstrap_library(
    name = "resolved-vars-language",
    srcs = ["resolved-vars-language.yaspl"],
    deps = [
        "//libraries:module-name",
        "//libraries:module-signature",
        "//libraries:top-level-name",
        "//libraries:types",
        "//libraries/core-language:sized-types",
        "//libraries/core-language:struct-info",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/yaspl:math-operations",
        "//libraries/yaspl:var",
    ],
)

yaspl_bootstrap_library(
    name = "source-language",
    srcs = ["source-language.yaspl"],
    deps = [
        "//libraries:module-name",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/yaspl:math-operations",
    ],
)

yaspl_bootstrap_library(
    name = "type-checked-language",
    srcs = ["type-checked-language.yaspl"],
    deps = [
        ":processed-structs-language",
        "//libraries:module-name",
        "//libraries:module-signature",
        "//libraries:types",
        "//libraries/core-language:sized-types",
        "//libraries/core-language:struct-info",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/yaspl:math-operations",
    ],
)
