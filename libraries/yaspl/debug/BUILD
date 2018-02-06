load("//libraries:yaspl.bzl", "yaspl_library")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_library(
    name = "intermediate-to-html",
    srcs = ["intermediate-to-html.yaspl"],
    deps = [
        "//libraries:intermediate-expression-printer",
        "//libraries:intermediate-language",
        "//libraries:module-name",
        "//libraries:pretty-print",
        "//libraries:types",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
        "//libraries/formats:html",
        "//libraries/formats:xml",
        "//libraries/yaspl:var",
    ],
)
