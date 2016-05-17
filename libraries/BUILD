load(":yaspl.bzl", "yaspl_library", "yaspl_binary", "yaspl_test",
     "yaspl_bootstrap_library")
package(
  default_visibility = ["//visibility:public"]
)

filegroup(
  name = "compiler_library_files",
  visibility = ["//visibility:public"],
  data = [":compiler_lib.srcs", ":compiler_lib.src.list"]
)



yaspl_binary(
  name = "compiler",
  main_module = "compiler",
  deps = [":compiler_lib"]
)

yaspl_binary(
  name = "code-http-server",
  main_module = "code_http_server",
  deps = [":code-http-server_lib"]
)

yaspl_binary(
  name = "http-server",
  main_module = "http_server",
  deps = [":http-server_lib"]
)

yaspl_binary(
  name = "library-compiler",
  main_module = "library_compiler",
  deps = [":library-compiler_lib"]
)

yaspl_binary(
  name = "print-stack-function",
  main_module = "print_stack_function",
  deps = [":print-stack-function_lib"]
)

yaspl_binary(
  name = "thread-example",
  main_module = "thread_example",
  deps = [":thread-example_lib"]
)

yaspl_binary(
  name = "udp-echo-server",
  main_module = "udp_echo_server",
  deps = [":udp-echo-server_lib"]
)


yaspl_library(
  name = "boolean",
  srcs = ["boolean.yaspl"],
)


yaspl_library(
  name = "buffered-port",
  srcs = ["buffered-port.yaspl"],
  deps = [
    ":bytes",
    ":maybe",
    ":tuples"
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
    ":intermediate-to-stack",
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
  name = "environment",
  srcs = ["environment.yaspl"],
  deps = [
    ":dict",
    ":bytes",
    ":list",
    ":maybe",
  ]
)

yaspl_bootstrap_library(
  name = "free-variables",
  srcs = ["free-variables.yaspl"],
  deps = [
    ":bytes",
    ":list",
    ":tuples",
    ":set",
    ":source-language",
  ]
)


yaspl_library(
  name = "code-http-server_lib",
  srcs = ["code-http-server.yaspl"],
  deps = [
    ":http-server_lib",
    ":join-list",
    ":print-stack-function_lib",
    ":stack-machine",
  ],
)

yaspl_library(
  name = "http-server_lib",
  srcs = ["http-server.yaspl"],
  deps = [
    ":buffered-port",
    ":io",
    ":ip",
    ":mach",
    ":tcp",
    ":tuples",
  ]
)


yaspl_bootstrap_library(
  name = "intermediate-language",
  srcs = ["intermediate-language.yaspl"],
  deps = [
    ":list",
    ":tuples",
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

yaspl_library(
  name = "ip",
  srcs = ["ip.yaspl"],
  deps = [
    ":bytes",
    ":list",
    ":numbers",
    ":regexp",
  ],
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

yaspl_test(
  name = "lexer-test",
  main_module = "lexer_test",
  srcs = ["lexer-test.yaspl"],
  deps = [
    ":boolean",
    ":either",
    ":list",
    ":lexer",
    ":yunit",
  ]
)

yaspl_bootstrap_library(
  name = "library-compiler_lib",
  srcs = ["library-compiler.yaspl"],
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
    ":intermediate-to-stack",
    ":stack-machine",
    ":stack-machine-optimizer",
    ":tuples",
    ":validator",
    ":x86-64-stack-machine",
 ]
)

yaspl_bootstrap_library(
  name = "list",
  srcs = ["list.yaspl"],
  deps = [":maybe"]
)

yaspl_library(
  name = "mach",
  srcs = ["mach.yaspl"],
  deps = [
    ":bytes",
    ":io",
    ":list",
    ":numbers",
  ]
)


yaspl_bootstrap_library(
  name = "maybe",
  srcs = ["maybe.yaspl"],
)

yaspl_bootstrap_library(
  name = "multi-set",
  srcs = ["multi-set.yaspl"],
  deps = [
    ":dict",
  ]
)


yaspl_bootstrap_library(
  name = "numbers",
  srcs = ["numbers.yaspl"],
)

yaspl_bootstrap_library(
  name = "prim-implementation",
  srcs = ["prim-implementation.yaspl"],
  deps = [
    ":dict",
    ":bytes",
    ":intermediate-to-module-signature",
    ":list",
    ":stack-machine",
    ":tuples",
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
    ":intermediate-to-stack",
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
    ":tuples",
    ":bytes",
  ]
)

yaspl_test(
  name = "regexp-test",
  main_module = "regexp_test",
  srcs = ["regexp-test.yaspl"],
  deps = [
    ":boolean",
    ":list",
    ":regexp",
    ":yunit",
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
    ":intermediate-to-stack",
    ":stack-machine",
    ":tuples",
 ]
)


yaspl_bootstrap_library(
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
    ":boolean",
    ":list",
    ":maybe",
    ":set",
    ":yunit",
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

yaspl_test(
  name = "sexp-parser-test",
  main_module = "sexp_parser_test",
  srcs = ["sexp-parser-test.yaspl"],
  deps = [
    ":boolean",
    ":either",
    ":list",
    ":sexp-parser",
    ":yunit",
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
   ":tuples",
  ]
)

yaspl_bootstrap_library(
  name = "source-to-intermediate-language",
  srcs = ["source-to-intermediate-language.yaspl"],
  deps = [
    ":dict",
    ":free-variables",
    ":intermediate-language",
    ":source-language",
    ":tuples",
  ]
)


yaspl_bootstrap_library(
  name = "intermediate-to-module-signature",
  srcs = ["intermediate-to-module-signature.yaspl"],
  deps = [
    ":bytes",
    ":dict",
    ":either",
    ":environment",
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
  name = "intermediate-to-stack",
  srcs = ["intermediate-to-stack.yaspl"],
  deps = [
    ":bytes",
    ":dict",
    ":either",
    ":environment",
    ":intermediate-to-module-signature",
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
  name = "tcp",
  srcs = ["tcp.yaspl"],
  deps = [
    ":ip",
    ":tuples",
  ]
)

yaspl_library(
  name = "thread-example_lib",
  srcs = ["thread-example.yaspl"],
  deps = [
    ":io",
    ":mach",
  ]
)


yaspl_bootstrap_library(
  name = "tuples",
  srcs = ["tuples.yaspl"],
)

yaspl_library(
  name = "udp",
  srcs = ["udp.yaspl"],
  deps = [
    ":ip",
    ":tuples",
  ]
)

yaspl_library(
  name = "udp-echo-server_lib",
  srcs = ["udp-echo-server.yaspl"],
  deps = [
    ":io",
    ":ip",
    ":udp",
    ":tuples",
  ]
)

yaspl_bootstrap_library(
  name = "validator",
  srcs = ["validator.yaspl"],
  deps = [
    ":bytes",
    ":dict",
    ":list",
    ":maybe",
    ":multi-set",
    ":source-language",
    ":intermediate-to-module-signature",
    ":tuples",
  ]
)

yaspl_bootstrap_library(
  name = "x86-64-stack-machine",
  srcs = ["x86-64-stack-machine.yaspl"],
  deps = [
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

yaspl_library(
  name = "yunit",
  srcs = ["yunit.yaspl"],
  deps = [
    ":io",
    ":list",
  ]
)

