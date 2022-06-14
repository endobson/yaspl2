package(
    default_visibility = ["//visibility:public"],
)

ALL_PACKAGES = [
    "//examples",
    "//examples/snake",
    "//libraries",
    "//libraries/algorithms",
    "//libraries/code-http-server",
    "//libraries/core-language",
    "//libraries/core-language/examples",
    "//libraries/data",
    "//libraries/database",
    "//libraries/os",
    "//libraries/prim-language",
    "//libraries/prim-language/examples",
    "//libraries/protocols/http",
    "//libraries/yaspl",
    "//libraries/yaspl/conversions",
    "//libraries/yaspl/frontend",
    "//libraries/yaspl/interpreter",
    "//libraries/yaspl/languages",
    "//libraries/yaspl/runtime",
    "//tools",
    "//tools/doc-server",
    "//tools/gcs-server",
    "//tools/objdump",
    "//tools/shell",
    "//tools/web-calculator",
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
