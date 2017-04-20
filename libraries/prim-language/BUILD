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
  name = "color-graph",
  srcs = ["color-graph.yaspl"],
  deps = [
    ":register-language",
    ":lower-x86-64",
    ":lowered-live-variables",
    "//libraries/data:bytes",
    "//libraries/data:either",
    "//libraries/data:list",
    "//libraries/data:set",
    "//libraries/data:dict",
    "//libraries/data:maybe",
    "//libraries/data:tuples",
    "//libraries/data:undirected-graph",
  ]
)

yaspl_test(
  name = "color-graph-test",
  main_module = "color_graph_test",
  srcs = ["color-graph-test.yaspl"],
  deps = [
    ":prim-language",
    ":prim-to-register",
    ":color-graph",
    ":lower-x86-64",
    ":lowered-live-variables",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries:yunit",
  ]
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
    "//libraries/data:set",
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
  name = "lowered-live-variables",
  srcs = ["lowered-live-variables.yaspl"],
  deps = [
    ":lowered-register-language",
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
    ":color-graph",
    ":lowered-live-variables",
    ":lower-x86-64",
    ":prim-language",
    ":register-language",
    ":prim-language-parser",
    ":prim-to-register",
    ":print-register-language",
    ":lowered-register-language",
    ":print-lowered-register-language",
    ":simplify-lowered-register-language",
    "//libraries:io",
    "//libraries:sexp-parser",
    "//libraries/data:bytes",
    "//libraries/data:dict",
    "//libraries/data:either",
    "//libraries/data:list",
    "//libraries/data:maybe",
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
  name = "print-lowered-register-language",
  srcs = ["print-lowered-register-language.yaspl"],
  deps = [
    ":prim-language",
    ":lowered-register-language",
    "//libraries/data:join-list",
    "//libraries/data:list",
    "//libraries/data:dict",
    "//libraries/data:bytes",
    "//libraries/data:numbers",
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
    "//libraries/data:dict",
    "//libraries/data:list",
    "//libraries/data:numbers",
  ]
)

yaspl_library(
  name = "simplify-lowered-register-language",
  srcs = ["simplify-lowered-register-language.yaspl"],
  deps = [
    ":lowered-register-language",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:dict",
  ]
)
