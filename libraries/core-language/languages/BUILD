load(
    "//libraries:yaspl.bzl",
    "yaspl_library",
)

package(
    default_visibility = ["//visibility:public"],
)

yaspl_library(
    name = "lifted-values-language",
    srcs = ["lifted-values-language.yaspl"],
    deps = [
        ":resolved-vars-language",
        "//libraries:module-name",
        "//libraries:top-level-name",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/yaspl:math-operations",
        "//libraries/yaspl:var",
    ],
)

yaspl_library(
    name = "resolved-imports-language",
    srcs = ["resolved-imports-language.yaspl"],
    deps = [
        "//libraries:module-name",
        "//libraries/core-language:source-language",
        "//libraries/data:dict",
        "//libraries/data:list",
    ],
)

yaspl_library(
    name = "resolved-types-language",
    srcs = ["resolved-types-language.yaspl"],
    deps = [
        ":resolved-imports-language",
        "//libraries:module-name",
        "//libraries:types",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/yaspl:math-operations",
    ],
)

yaspl_library(
    name = "resolved-vars-language",
    srcs = ["resolved-vars-language.yaspl"],
    deps = [
        ":resolved-imports-language",
        "//libraries:module-name",
        "//libraries:top-level-name",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/yaspl:math-operations",
        "//libraries/yaspl:var",
    ],
)
