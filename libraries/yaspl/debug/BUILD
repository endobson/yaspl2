load("//libraries:yaspl.bzl", "yaspl_library")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_library(
    name = "intermediate-to-html",
    srcs = ["intermediate-to-html.yaspl"],
    deps = [
        "//libraries:module-name",
        "//libraries:intermediate-language",
        "//libraries:types",
        "//libraries/data:list",
        "//libraries/data:join-list",
        "//libraries/data:tuples",
        "//libraries/formats:html",
        "//libraries/formats:xml",
    ],
)
