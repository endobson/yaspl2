load("//libraries:yaspl.bzl", "yaspl_library", "yaspl_binary")

yaspl_binary(
  name = "rewrite-imports",
  main_module = "rewrite_imports",
  deps = [":rewrite-imports-main"]
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
