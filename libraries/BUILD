load(":yaspl.bzl", "yaspl_library", "yaspl_binary", "yaspl_test",
     "yaspl_bootstrap_library")
package(
  default_visibility = ["//visibility:public"]
)

filegroup(
  name = "compiler_library_files",
  visibility = ["//visibility:public"],
  data = [":compiler_lib.srcs"]
)

genquery(
  name = "compiler_library_files_file",
  visibility = ["//visibility:public"],
  scope = [":compiler_library_files"],
  expression = "kind(\"source file\", deps(//libraries:compiler_library_files))"
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


yaspl_bootstrap_library(
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


yaspl_bootstrap_library(
  name = "bytes",
  srcs = ["bytes.yaspl"],
  deps = [
    ":list"
  ]
)

yaspl_bootstrap_library(
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

yaspl_bootstrap_library(
  name = "dict",
  srcs = ["dict.yaspl"],
  deps = [
    ":list",
    ":maybe",
    ":tuples",
  ]
)

yaspl_bootstrap_library(
  name = "either",
  srcs = ["either.yaspl"],
)

yaspl_bootstrap_library(
  name = "intermediate-language",
  srcs = ["intermediate-language.yaspl"],
  deps = [
    ":list",
    ":source-language",
  ]
)

yaspl_bootstrap_library(
  name = "io",
  srcs = ["io.yaspl"],
  deps = [
    ":bytes"
  ]
)

yaspl_bootstrap_library(
  name = "join-list",
  srcs = ["join-list.yaspl"],
  deps = [":list"]
)

yaspl_bootstrap_library(
  name = "lexer",
  srcs = ["lexer.yaspl"],
  deps = [
    ":bytes",
    ":io",
    ":numbers",
  ]
)

yaspl_bootstrap_library(
  name = "list",
  srcs = ["list.yaspl"],
  deps = [":maybe"]
)

yaspl_bootstrap_library(
  name = "maybe",
  srcs = ["maybe.yaspl"],
)

yaspl_bootstrap_library(
  name = "numbers",
  srcs = ["numbers.yaspl"],
)

yaspl_bootstrap_library(
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
    ":compiler_lib",
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

yaspl_bootstrap_library(
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


yaspl_bootstrap_library(
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

yaspl_bootstrap_library(
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

yaspl_bootstrap_library(
  name = "source-to-intermediate-language",
  srcs = ["source-to-intermediate-language.yaspl"],
  deps = [
    ":source-language",
    ":intermediate-language"
  ]
)


yaspl_bootstrap_library(
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
    ":source-to-intermediate-language",
    ":stack-machine",
    ":tuples",
  ]
)

yaspl_bootstrap_library(
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

yaspl_bootstrap_library(
  name = "tuples",
  srcs = ["tuples.yaspl"],
)

yaspl_bootstrap_library(
  name = "validator",
  srcs = ["validator.yaspl"],
  deps = [
    ":list",
    ":maybe",
    ":source-language",
    ":source-to-stack",
  ]
)

yaspl_bootstrap_library(
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
