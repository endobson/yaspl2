load(":yaspl.bzl", "yaspl_library", "yaspl_binary", "yaspl_test")
package(
  default_visibility = ["//visibility:public"]
)


filegroup(
  name = "compiler_library_files",
  visibility = ["//visibility:public"],
  srcs = [
    ":arithmetic-expr.yaspl",
    ":bytes.yaspl",
    ":compiler.yaspl",
    ":dict.yaspl",
    ":either.yaspl",
    ":io.yaspl",
    ":join-list.yaspl",
    ":lexer.yaspl",
    ":list.yaspl",
    ":maybe.yaspl",
    ":numbers.yaspl",
    ":prim-implementation.yaspl",
    ":sexp-parser.yaspl",
    ":source-language.yaspl",
    ":source-to-stack.yaspl",
    ":stack-machine.yaspl",
    ":stack-machine-optimizer.yaspl",
    ":tuples.yaspl",
    ":validator.yaspl",
    ":x86-64-stack-machine.yaspl",
  ]
)

yaspl_binary(
  name = "compiler",
  main_module = "compiler",
  deps = [":compiler_lib"]
)


yaspl_binary(
  name = "print-stack-function",
  main_module = "print_stack_function",
  deps = [":print-stack-function_lib"]
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
    ":maybe",
    ":prim-implementation",
    ":sexp-parser",
    ":source-language",
    ":source-to-stack",
    ":stack-machine",
    ":stack-machine-optimizer",
    ":tuples",
    ":validator",
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
  name = "intermediate-language",
  srcs = ["intermediate-language.yaspl"],
  deps = [
    ":list",
    ":source-language",
  ]
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
  name = "print-stack-function_lib",
  srcs = ["print-stack-function.yaspl"],
  deps = [
    ":bytes",
    ":compiler",
    ":dict",
    ":either",
    ":io",
    ":list",
    ":prim-implementation",
    ":sexp-parser",
    ":source-language",
    ":source-to-stack",
    ":stack-machine-optimizer",
    ":stack-machine",
    ":tuples",
 ]
)



yaspl_library(
  name = "regexp",
  srcs = ["regexp.yaspl"],
  deps = [
    ":list",
    ":maybe",
  ]
)

yaspl_test(
  name = "regexp-test",
  main_module = "regexp_test",
  srcs = ["regexp-test.yaspl"],
  deps = [
    ":io",
    ":list",
    ":maybe",
    ":regexp",
  ]
)

yaspl_library(
  name = "stack-machine-optimizer",
  srcs = ["stack-machine-optimizer.yaspl"],
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
 ]
)


yaspl_library(
  name = "set",
  srcs = ["set.yaspl"],
  deps = [
    ":list",
  ]
)

yaspl_test(
  name = "set-test",
  main_module = "set_test",
  srcs = ["set-test.yaspl"],
  deps = [
    ":io",
    ":list",
    ":maybe",
    ":set",
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
  name = "source-to-intermediate-language",
  srcs = ["source-to-intermediate-language.yaspl"],
  deps = [
    ":source-language",
    ":intermediate-language"
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
  name = "validator",
  srcs = ["validator.yaspl"],
  deps = [
    ":list",
    ":maybe",
    ":source-language",
    ":source-to-stack",
  ]
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
