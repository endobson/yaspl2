load("//libraries:yaspl.bzl", "yaspl_library")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_library(
    name = "intermediate-to-html",
    srcs = ["intermediate-to-html.yaspl"],
    deps = [
        "//libraries:intermediate-language",
        "//libraries:module-name",
        "//libraries:types",
        "//libraries:intermediate-expression-printer",
        "//libraries:pretty-print",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
        "//libraries/formats:html",
        "//libraries/formats:xml",
    ],
)
