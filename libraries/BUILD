load(":yaspl.bzl", "yaspl_library")

filegroup(
  name = "compiler_library_files",
  visibility = ["//visibility:public"],
  srcs = glob(["*.yaspl"])
)





yaspl_library(
  name = "arithmetic-expr",
  srcs = ["arithmetic-expr.yaspl"],
  deps = [
    ":sexp-parser",
    ":io",
    ":either",
    ":list",
    ":bytes"
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
  name = "compiler",
  srcs = ["compiler.yaspl"],
  deps = [
    ":io",
    ":list",
    ":dict",
    ":tuples",
    ":bytes",
    ":source-language",
    ":sexp-parser",
    ":source-to-stack",
    ":stack-machine",
    ":x86-64-stack-machine",
    ":prim-implementation",
    ":either"
 ]
)

yaspl_library(
  name = "dict",
  srcs = ["dict.yaspl"],
  deps = [
    ":maybe",
    ":tuples",
    ":list"
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
    ":numbers",
    ":bytes",
    ":io"
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
    ":stack-machine"
  ]
)

yaspl_library(
  name = "sexp-parser",
  srcs = ["sexp-parser.yaspl"],
  deps = [
    ":lexer",
    ":numbers",
    ":io",
    ":either",
    ":maybe",
    ":list"
  ]
)

yaspl_library(
  name = "source-language",
  srcs = ["source-language.yaspl"],
  deps = [
   ":sexp-parser",
   ":io",
   ":either",
   ":maybe",
   ":list"
  ]
)

yaspl_library(
  name = "source-to-stack",
  srcs = ["source-to-stack.yaspl"],
  deps = [
    ":list",
    ":bytes",
    ":either",
    ":dict",
    ":maybe",
    ":tuples",
    ":join-list",
    ":stack-machine",
    ":source-language"
  ]
)

yaspl_library(
  name = "stack-machine",
  srcs = ["stack-machine.yaspl"],
  deps = [
    ":arithmetic-expr",
    ":list",
    ":tuples",
    ":join-list",
    ":bytes",
    ":sexp-parser",
    ":io",
    ":numbers"
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
    ":prim-implementation",
    ":list",
    ":numbers",
    ":io",
    ":arithmetic-expr",
    ":sexp-parser",
    ":bytes",
    ":stack-machine",
    ":either"
  ]
)
