load("//tools:yaspl-module-index.bzl", "yaspl_module_index_rule")
load("//tools:yaspl-missing-imports.bzl", "yaspl_missing_imports_rule")
load("//tools:yaspl-lint.bzl", "yaspl_lint_rule")

exports_files(["racket.bzl"])

ALL_PACKAGES = [
    "//examples",
    "//libraries",
    "//libraries/algorithms",
    "//libraries/data",
    "//libraries/prim-language",
    "//tools",
    "//tools/shell",
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

yaspl_module_index_rule(
    name = "module_index",
    visibility = ["//visibility:public"],
    deps = [
        ":all_binaries",
        ":all_tests",
    ],
)

yaspl_missing_imports_rule(
    name = "missing_imports",
    deps = [
        ":all_binaries",
        ":all_tests",
    ],
)

yaspl_lint_rule(
    name = "lint",
    deps = [
        ":all_binaries",
        ":all_tests",
    ],
)
