load("//libraries:yaspl.bzl", "yaspl_library")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_library(
    name = "intermediate-to-html",
    srcs = ["intermediate-to-html.yaspl"],
    deps = [
        "//libraries:intermediate-language",
        "//libraries/data:list",
        "//libraries/formats:html",
        "//libraries/formats:xml",
    ],
)
