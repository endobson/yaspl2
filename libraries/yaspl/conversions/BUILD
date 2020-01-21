load(
    "//libraries:yaspl.bzl",
    "yaspl_bootstrap_library",
    "yaspl_test",
)

package(
    default_visibility = ["//visibility:public"],
)

yaspl_bootstrap_library(
    name = "source-language-parser",
    srcs = ["source-language-parser.yaspl"],
    deps = [
        "//libraries:module-name",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
        "//libraries/formats:sexp",
        "//libraries/yaspl/languages:source-language",
    ],
)

yaspl_test(
    name = "source-language-parser-test",
    size = "small",
    srcs = ["source-language-parser-test.yaspl"],
    deps = [
        ":source-language-parser",
        "//libraries:sexp-parser",
        "//libraries:yunit",
        "//libraries/data:bytes",
        "//libraries/data:either",
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
