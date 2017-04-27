load("//libraries:yaspl.bzl", "yaspl_library", "yaspl_binary")

yaspl_binary(
  name = "rewrite-imports",
  main_module = "rewrite_imports",
  deps = [":rewrite-imports-main"]
)

yaspl_binary(
  name = "aspect-linter",
  main_module = "aspect_linter_main",
  deps = [":aspect-linter-main"],
  visibility = ["//visibility:public"],
)


yaspl_library(
  name = "rewrite-imports-main",
  srcs = ["rewrite-imports.yaspl"],
  deps = [
    "//libraries:io",
    "//libraries:compiler_lib",
    "//libraries:source-language",
    "//libraries:sexp-parser",
    "//libraries/data:either",
    "//libraries/data:bytes",
    "//libraries/data:list",
    "//libraries/data:tuples",
 ]
)


yaspl_library(
  name = "aspect-linter-main",
  srcs = ["aspect-linter-main.yaspl"],
  deps = [
    ":linter_lib",
    "//libraries:io",
    "//libraries/data:either",
    "//libraries/data:list",
  ]
)


yaspl_library(
  name = "linter_lib",
  srcs = ["linter.yaspl"],
  deps = [
    "//libraries:io",
    "//libraries:sexp-parser",
    "//libraries:source-language",
    "//libraries:unused-bindings",
    "//libraries:module-signature",
    "//libraries:module-signature-serialization",
    "//libraries:module-header-lint",
    "//libraries/data:bytes",
    "//libraries/data:either",
    "//libraries/data:list",
    "//libraries/data:set",
  ]
)
