load(
    "//libraries:yaspl.bzl",
    "yaspl_bootstrap_library",
)

package(
    default_visibility = ["//visibility:public"],
)

yaspl_bootstrap_library(
    name = "expanded-datatypes-language",
    srcs = ["expanded-datatypes-language.yaspl"],
    deps = [
        ":resolved-types-language",
        "//libraries:module-name",
        "//libraries:module-signature",
        "//libraries:top-level-name",
        "//libraries:types",
        "//libraries:variant-info",
        "//libraries/data:dict",
        "//libraries/data:list",
    ],
)

yaspl_bootstrap_library(
    name = "expanded-patterns-language",
    srcs = ["expanded-patterns-language.yaspl"],
    deps = [
        ":expanded-varargs-language",
        "//libraries:module-name",
        "//libraries:top-level-name",
        "//libraries:types",
        "//libraries:variant-info",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/yaspl:var",
    ],
)

yaspl_bootstrap_library(
    name = "expanded-varargs-language",
    srcs = ["expanded-varargs-language.yaspl"],
    deps = [
        "//libraries:module-name",
        ":resolved-vars-language",
        "//libraries:top-level-name",
        "//libraries:types",
        "//libraries/data:list",
        "//libraries/data:maybe",
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
        "//libraries:top-level-name",
        "//libraries:types",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:source-location",
    ],
)

yaspl_bootstrap_library(
    name = "resolved-types-language",
    srcs = ["resolved-types-language.yaspl"],
    deps = [
        ":resolved-imports-language",
        "//libraries:module-name",
        "//libraries:module-signature",
        "//libraries:top-level-name",
        "//libraries:types",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
    ],
)

yaspl_bootstrap_library(
    name = "resolved-vars-language",
    srcs = ["resolved-vars-language.yaspl"],
    deps = [
        "//libraries:module-name",
        "//libraries:module-signature",
        "//libraries:top-level-name",
        "//libraries:type-checked-language",
        "//libraries:types",
        "//libraries:variant-info",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/yaspl:var",
    ],
)

yaspl_bootstrap_library(
    name = "source-language",
    srcs = ["source-language.yaspl"],
    deps = [
        "//libraries:module-name",
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
