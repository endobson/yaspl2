load("//tools:yaspl-module-index.bzl", "yaspl_module_index_rule")
load("//tools:yaspl-missing-dependencies.bzl", "yaspl_missing_dependencies_rule")
load("//tools:yaspl-remove-unused-dependencies.bzl", "yaspl_remove_unused_dependencies_rule")
load("//tools:yaspl-lint.bzl", "yaspl_lint_rule")
load("//tools:yaspl-clean-up-imports.bzl", "yaspl_clean_up_imports_rule")
load("//tools:yaspl-validate-ir.bzl", "yaspl_validate_ir_rule")

yaspl_module_index_rule(
    name = "module_index",
    visibility = ["//visibility:public"],
    deps = [
        "//:all_binaries",
        "//:all_tests",
    ],
)

yaspl_missing_dependencies_rule(
    name = "missing_dependencies",
    deps = [
        "//:all_binaries",
        "//:all_tests",
    ],
)

yaspl_remove_unused_dependencies_rule(
    name = "remove_unused_dependencies",
    deps = [
        "//:all_binaries",
        "//:all_tests",
    ],
)

yaspl_lint_rule(
    name = "lint",
    deps = [
        "//:all_binaries",
        "//:all_tests",
    ],
)

yaspl_clean_up_imports_rule(
    name = "clean_up_imports",
    deps = [
        "//:all_binaries",
        "//:all_tests",
    ],
)
