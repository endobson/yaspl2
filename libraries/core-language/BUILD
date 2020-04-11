load(
    "//libraries:yaspl.bzl",
    "yaspl_library",
    "yaspl_test",
)

package(
    default_visibility = ["//visibility:public"],
)

test_suite(
    name = "package_tests",
    testonly = 0,
)


yaspl_library(
    name = "lexer",
    srcs = ["lexer.yaspl"],
    deps = [
        "//libraries:byte-escapes",
        "//libraries:lexer",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:numbers",
        "//libraries/data:source-location",
    ],
)


yaspl_test(
    name = "lexer-test",
    size = "small",
    srcs = ["lexer-test.yaspl"],
    deps = [
        ":lexer",
        "//libraries:sexp-lexer",
        "//libraries:yunit",
        "//libraries/data:bytes",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
    ],
)


filegroup(
    name = "package_binaries",
    srcs = [
    ],
)
