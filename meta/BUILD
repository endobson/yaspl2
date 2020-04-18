load("//tools:yaspl-module-index.bzl", "yaspl_module_index_rule")
load("//tools:yaspl-missing-dependencies.bzl", "yaspl_missing_dependencies_rule")
load("//tools:yaspl-remove-unused-dependencies.bzl", "yaspl_remove_unused_dependencies_rule")
load("//tools:yaspl-lint.bzl", "yaspl_lint_rule")
load("//tools:yaspl-clean-up-imports.bzl", "yaspl_clean_up_imports_rule")
load("//tools:yaspl-validate-ir.bzl", "yaspl_validate_ir_rule")
load("//tools:yaspl-exported-values.bzl", "yaspl_exported_values_rule")
load("//tools:yaspl-module-dependency-graph.bzl", "yaspl_module_dependency_graph_rule")

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

sh_test(
    name = "lint-test",
    srcs = ["lint-test.sh"],
    data = [":lint.lint"],
)

yaspl_clean_up_imports_rule(
    name = "clean_up_imports",
    deps = [
        "//:all_binaries",
        "//:all_tests",
    ],
)

yaspl_exported_values_rule(
    name = "exported_values",
    deps = [
        "//:all_binaries",
        "//:all_tests",
    ],
)

yaspl_module_dependency_graph_rule(
    name = "module_dependencies",
    deps = [
        "//:all_binaries",
        "//:all_tests",
    ],
)

sh_test(
    name = "validate-ir-test",
    srcs = ["validate-ir-test.sh"],
    data = [":validate_ir.validate_ir"],
)

yaspl_validate_ir_rule(
    name = "validate_ir",
    deps = [
        "//:all_binaries",
        "//:all_tests",
    ],
)

test_suite(
    name = "tests",
)
