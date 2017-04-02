load("//libraries:yaspl.bzl", "yaspl_library", "yaspl_test", "yaspl_binary")
package(
  default_visibility = ["//visibility:public"]
)

yaspl_binary(
  name = "demo",
  main_module = "prim_language_demo",
  deps = [":prim-language-demo"]
)

yaspl_library(
  name = "live-variables",
  srcs = ["live-variables.yaspl"],
  deps = [
    ":register-language",
    "//libraries:graphviz",
    "//libraries/data:list",
    "//libraries/data:set",
    "//libraries/data:dict",
    "//libraries/data:maybe",
    "//libraries/data:tuples",
    "//libraries/data:bytes",
    "//libraries/data:numbers",
    "//libraries/data:undirected-graph",
    "//libraries/data:join-list",
  ]
)

yaspl_library(
  name = "lower-x86-64",
  srcs = ["lower-x86-64.yaspl"],
  deps = [
    ":register-language",
    ":lowered-register-language",
    "//libraries/data:dict",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:tuples",
  ]
)

yaspl_library(
  name = "lowered-register-language",
  srcs = ["lowered-register-language.yaspl"],
  deps = [
    ":prim-language",
    "//libraries/data:list",
    "//libraries/data:dict",
  ]
)


yaspl_library(
  name = "prim-language",
  srcs = ["prim-language.yaspl"],
  deps = [
    "//libraries/data:list",
  ]
)

yaspl_library(
  name = "prim-language-demo",
  srcs = ["prim-language-demo.yaspl"],
  deps = [
    ":live-variables",
    ":prim-language",
    ":register-language",
    ":prim-language-parser",
    ":prim-to-register",
    ":print-register-language",
    "//libraries:io",
    "//libraries:sexp-parser",
    "//libraries/data:either",
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

yaspl_library(
  name = "print-register-language",
  srcs = ["print-register-language.yaspl"],
  deps = [
    ":prim-language",
    ":register-language",
    "//libraries/data:join-list",
    "//libraries/data:list",
    "//libraries/data:dict",
    "//libraries/data:bytes",
    "//libraries/data:numbers",
  ]
)

yaspl_library(
  name = "prim-to-register",
  srcs = ["prim-to-register.yaspl"],
  deps = [
    ":prim-language",
    ":register-language",
    "//libraries/data:boolean",
    "//libraries/data:bytes",
    "//libraries/data:dict",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:set",
    "//libraries/data:tuples",
  ]
)


yaspl_library(
  name = "register-language",
  srcs = ["register-language.yaspl"],
  deps = [
    ":prim-language",
    "//libraries/data:bytes",
    "//libraries/data:list",
    "//libraries/data:dict",
  ]
)
