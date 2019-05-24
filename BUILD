package(
    default_visibility = ["//visibility:public"],
)

exports_files(["racket.bzl"])

ALL_PACKAGES = [
    "//examples",
    "//libraries",
    "//libraries/algorithms",
    "//libraries/code-http-server",
    "//libraries/data",
    "//libraries/prim-language",
    "//libraries/prim-language/examples",
    "//libraries/protocols/http",
    "//tools",
    "//tools/shell",
    "//tools/web-calculator",
    "//tools/doc-server",
    "//web-assembly",
]

test_suite(
    name = "all_tests",
    testonly = 0,
    tests = [pkg + ":package_tests" for pkg in ALL_PACKAGES],
)

filegroup(
    name = "all_binaries",
    srcs = [pkg + ":package_binaries" for pkg in ALL_PACKAGES],
)
