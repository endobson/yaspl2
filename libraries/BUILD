load(":yaspl.bzl", "yaspl_library", "yaspl_binary")
package(
  default_visibility = ["//visibility:public"]
)


filegroup(
  name = "compiler_library_files",
  visibility = ["//visibility:public"],
  srcs = glob(["*.yaspl"])
)

yaspl_binary(
  name = "compiler",
  main_module = "compiler",
  deps = [":compiler_lib"]
)


yaspl_library(
  name = "arithmetic-expr",
  srcs = ["arithmetic-expr.yaspl"],
  deps = [
    ":bytes",
    ":either",
    ":io",
    ":list",
    ":sexp-parser",
  ]
)


yaspl_library(
  name = "bytes",
  srcs = ["bytes.yaspl"],
  deps = [
    ":list"
  ]
)

yaspl_library(
  name = "compiler_lib",
  srcs = ["compiler.yaspl"],
  deps = [
    ":bytes",
    ":dict",
    ":either",
    ":io",
    ":list",
    ":prim-implementation",
    ":sexp-parser",
    ":source-language",
    ":source-to-stack",
    ":stack-machine",
    ":tuples",
    ":x86-64-stack-machine",
 ]
)

yaspl_library(
  name = "dict",
  srcs = ["dict.yaspl"],
  deps = [
    ":list",
    ":maybe",
    ":tuples",
  ]
)

yaspl_library(
  name = "either",
  srcs = ["either.yaspl"],
)

yaspl_library(
  name = "io",
  srcs = ["io.yaspl"],
  deps = [
    ":bytes"
  ]
)

yaspl_library(
  name = "join-list",
  srcs = ["join-list.yaspl"],
  deps = [":list"]
)

yaspl_library(
  name = "lexer",
  srcs = ["lexer.yaspl"],
  deps = [
    ":bytes",
    ":io",
    ":numbers",
  ]
)

yaspl_library(
  name = "list",
  srcs = ["list.yaspl"],
  deps = [":maybe"]
)

yaspl_library(
  name = "maybe",
  srcs = ["maybe.yaspl"],
)

yaspl_library(
  name = "numbers",
  srcs = ["numbers.yaspl"],
)

yaspl_library(
  name = "prim-implementation",
  srcs = ["prim-implementation.yaspl"],
  deps = [
    ":list",
    ":stack-machine",
  ]
)

yaspl_library(
  name = "sexp-parser",
  srcs = ["sexp-parser.yaspl"],
  deps = [
    ":either",
    ":io",
    ":lexer",
    ":list",
    ":maybe",
    ":numbers",
  ]
)

yaspl_library(
  name = "source-language",
  srcs = ["source-language.yaspl"],
  deps = [
   ":either",
   ":io",
   ":list",
   ":maybe",
   ":sexp-parser",
  ]
)

yaspl_library(
  name = "source-to-stack",
  srcs = ["source-to-stack.yaspl"],
  deps = [
    ":bytes",
    ":dict",
    ":either",
    ":join-list",
    ":list",
    ":maybe",
    ":source-language",
    ":stack-machine",
    ":tuples",
  ]
)

yaspl_library(
  name = "stack-machine",
  srcs = ["stack-machine.yaspl"],
  deps = [
    ":arithmetic-expr",
    ":bytes",
    ":io",
    ":join-list",
    ":list",
    ":numbers",
    ":sexp-parser",
    ":tuples",
  ],
)



yaspl_library(
  name = "tuples",
  srcs = ["tuples.yaspl"],
)

yaspl_library(
  name = "x86-64-stack-machine",
  srcs = ["x86-64-stack-machine.yaspl"],
  deps = [
    ":arithmetic-expr",
    ":bytes",
    ":either",
    ":io",
    ":list",
    ":numbers",
    ":prim-implementation",
    ":sexp-parser",
    ":stack-machine",
  ]
)
