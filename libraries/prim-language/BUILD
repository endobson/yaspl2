load("//libraries:yaspl.bzl", "yaspl_library", "yaspl_test", "yaspl_bootstrap_library")
package(
  default_visibility = ["//visibility:public"]
)

yaspl_library(
  name = "prim-language",
  srcs = ["prim-language.yaspl"],
  deps = [
    "//libraries/data:list",
  ]
)

yaspl_library(
  name = "prim-language-parser",
  srcs = ["prim-language-parser.yaspl"],
  deps = [
    ":prim-language",
    "//libraries:sexp-parser",
    "//libraries/data:either",
    "//libraries/data:list",
  ]
)
