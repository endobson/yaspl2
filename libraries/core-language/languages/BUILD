load(
    "//libraries:yaspl.bzl",
    "yaspl_library",
)

package(
    default_visibility = ["//visibility:public"],
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
