load("//libraries:yaspl.bzl", "yaspl_library", "yaspl_binary")

yaspl_binary(
    name = "rewrite-imports",
    main_module = "rewrite_imports",
    deps = [":rewrite-imports-main"],
)

yaspl_binary(
    name = "aspect-linter",
    main_module = "aspect_linter_main",
    visibility = ["//visibility:public"],
    deps = [":aspect-linter-main"],
)

yaspl_library(
    name = "rewrite-imports-main",
    srcs = ["rewrite-imports.yaspl"],
    deps = [
        "//libraries:compiler_lib",
        "//libraries:io",
        "//libraries:module-name",
        "//libraries:sexp-parser",
        "//libraries:source-language",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:tuples",
    ],
)

yaspl_library(
    name = "aspect-linter-main",
    srcs = ["aspect-linter-main.yaspl"],
    deps = [
        ":linter_lib",
        "//libraries:io",
        "//libraries/data:either",
        "//libraries/data:list",
    ],
)

yaspl_library(
    name = "linter_lib",
    srcs = ["linter.yaspl"],
    deps = [
        "//libraries:io",
        "//libraries:module-header-lint",
        "//libraries:module-name",
        "//libraries:module-signature",
        "//libraries:module-signature-serialization",
        "//libraries:sexp-parser",
        "//libraries:source-language",
        "//libraries:unused-bindings",
        "//libraries/data:bytes",
        "//libraries/data:indexed-set",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:set",
    ],
)
