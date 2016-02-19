load(":yaspl.bzl", "yaspl_library")

filegroup(
  name = "compiler_library_files",
  visibility = ["//visibility:public"],
  srcs = glob(["*.yaspl"])
)




yaspl_library(
  name = "bytes",
  srcs = ["bytes.yaspl"],
  deps = []
)

yaspl_library(
  name = "compiler",
  srcs = ["compiler.yaspl"],
  deps = []
)

yaspl_library(
  name = "dict",
  srcs = ["dict.yaspl"],
  deps = []
)

yaspl_library(
  name = "either",
  srcs = ["either.yaspl"],
)

yaspl_library(
  name = "io",
  srcs = ["io.yaspl"],
)

yaspl_library(
  name = "join-list",
  srcs = ["join-list.yaspl"],
)

yaspl_library(
  name = "lexer",
  srcs = ["lexer.yaspl"],
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
)

yaspl_library(
  name = "sexp-parser",
  srcs = ["sexp-parser.yaspl"],
)

yaspl_library(
  name = "source-language",
  srcs = ["source-language.yaspl"],
)

yaspl_library(
  name = "source-to-stack",
  srcs = ["source-to-stack.yaspl"],
)

yaspl_library(
  name = "tuples",
  srcs = ["tuples.yaspl"],
)

yaspl_library(
  name = "x86-64-stack-machine",
  srcs = ["x86-64-stack-machine.yaspl"],
)
