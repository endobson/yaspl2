load(":yaspl.bzl", "yaspl_library", "yaspl_binary", "yaspl_test",
     "yaspl_bootstrap_library")
package(
  default_visibility = ["//visibility:public"]
)

filegroup(
  name = "compiler_library_files",
  visibility = ["//visibility:public"],
  data = [":compiler-main.srcs", ":compiler-main.src.list"]
)

filegroup(
  name = "library_compiler_library_files",
  visibility = ["//visibility:public"],
  data = [":library-compiler-main.srcs", ":library-compiler-main.src.list"]
)

filegroup(
  name = "main_stub_library_files",
  visibility = ["//visibility:public"],
  data = [":main-stub_lib.srcs", ":main-stub_lib.src.list"]
)


yaspl_binary(
  name = "compiler",
  main_module = "compiler_main",
  deps = [":compiler-main"]
)

yaspl_binary(
  name = "code-http-server",
  main_module = "code_http_server",
  deps = [":code-http-server_lib"]
)

yaspl_binary(
  name = "extra-action-parser",
  main_module = "extra_action_parser",
  deps = [":extra-action-parser_lib"]
)

yaspl_binary(
  name = "http-server",
  main_module = "http_server",
  deps = [":http-server_lib"]
)

yaspl_binary(
  name = "library-compiler",
  main_module = "library_compiler_main",
  deps = [":library-compiler-main"]
)

yaspl_binary(
  name = "main-stub",
  main_module = "main_stub",
  deps = [":main-stub_lib"]
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
  name = "true",
  main_module = "true",
  deps = [":true_lib"]
)

yaspl_binary(
  name = "wc",
  main_module = "wc",
  deps = [":wc_lib"]
)


yaspl_binary(
  name = "hello-world",
  main_module = "hello_world",
  deps = [":hello-world_lib"]
)


yaspl_binary(
  name = "udp-echo-server",
  main_module = "udp_echo_server",
  deps = [":udp-echo-server_lib"]
)


yaspl_bootstrap_library(
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

yaspl_test(
  name = "bytes-test",
  main_module = "bytes_test",
  srcs = ["bytes-test.yaspl"],
  deps = [
    ":boolean",
    ":bytes",
    ":list",
    ":yunit",
  ]
)

yaspl_bootstrap_library(
  name = "compiler-main",
  srcs = ["compiler-main.yaspl"],
  deps = [
    ":bytes",
    ":dict",
    ":either",
    ":io",
    ":compiler_lib",
    ":list",
    ":maybe",
    ":prim-implementation",
    ":sexp-parser",
    ":source-language",
    ":intermediate-to-stack",
    ":stack-machine",
    ":stack-machine-optimizer",
    ":tuples",
    ":type-checker",
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

yaspl_library(
  name = "extra-action-parser_lib",
  srcs = ["extra-action-parser.yaspl"],
  deps = [
    ":bytes",
    ":either",
    ":io",
    ":list",
    ":mach-o_lib",
    ":mach-o-writer2_lib",
    ":numbers",
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
    ":buffered-port",
    ":bytes",
    ":either",
    ":http-server_lib",
    ":io",
    ":ip",
    ":join-list",
    ":list",
    ":mach",
    ":maybe",
    ":numbers",
    ":print-stack-function_lib",
    ":regexp",
    ":stack-machine",
    ":tcp",
    ":tuples",
    ":x86-64-stack-machine",
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
    ":types",
  ]
)

yaspl_bootstrap_library(
  name = "intermediate-to-static-module-signature",
  srcs = ["intermediate-to-static-module-signature.yaspl"],
  deps = [
    ":bytes",
    ":dict",
    ":intermediate-language",
    ":list",
    ":types",
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

yaspl_test(
  name = "large-function-test",
  main_module = "large_function_test",
  srcs = ["large-function-test.yaspl"],
  deps = [
    ":list",
    ":yunit",
  ]
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
  name = "library-compiler-main",
  srcs = ["library-compiler-main.yaspl"],
  deps = [
    ":bytes",
    ":dict",
    ":either",
    ":io",
    ":compiler_lib",
    ":list",
    ":maybe",
    ":prim-implementation",
    ":sexp-parser",
    ":source-language",
    ":intermediate-to-stack",
    ":stack-machine",
    ":stack-machine-optimizer",
    ":tuples",
    ":type-checker",
    ":validator",
    ":x86-64-stack-machine",
    ":mach-o-writer2_lib",
 ]
)

yaspl_bootstrap_library(
  name = "compiler_lib",
  srcs = ["compiler.yaspl"],
  deps = [
    ":bytes",
    ":dict",
    ":either",
    ":list",
    ":maybe",
    ":prim-implementation",
    ":source-language",
    ":intermediate-to-stack",
    ":stack-machine",
    ":stack-machine-optimizer",
    ":tuples",
    ":validator",
 ]
)


yaspl_bootstrap_library(
  name = "list",
  srcs = ["list.yaspl"],
  deps = [
    ":either",
    ":maybe"
  ]
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
  name = "mach-o_lib",
  srcs = ["mach-o.yaspl"],
  deps = [
    ":boolean",
    ":bytes",
    ":either",
    ":io",
    ":numbers",
  ]
)

yaspl_bootstrap_library(
  name = "mach-o-writer_lib",
  srcs = ["mach-o-writer.yaspl"],
  deps = [
    ":bytes",
    ":dict",
    ":io",
    ":list",
    ":numbers",
  ]
)

yaspl_bootstrap_library(
  name = "mach-o-writer2_lib",
  srcs = ["mach-o-writer2.yaspl"],
  deps = [
    ":io",
    ":bytes",
    ":list",
    ":x86-64-stack-machine",
    ":dict",
    ":prim-implementation",
    ":source-language",
    ":sexp-parser",
    ":either",
    ":mach-o-writer_lib",
    ":mach-o_lib",
  ]
)



yaspl_bootstrap_library(
  name = "main-stub_lib",
  srcs = ["main-stub.yaspl"],
  deps = [
    ":bytes",
    ":io",
    ":list",
    ":stack-machine-optimizer",
    ":x86-64-stack-machine",
    ":mach-o-writer2_lib",
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
    ":either",
    ":io",
    ":list",
    ":prim-implementation",
    ":compiler_lib",
    ":sexp-parser",
    ":source-language",
    ":intermediate-to-stack",
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
    ":bytes",
    ":dict",
    ":free-variables",
    ":intermediate-language",
    ":intermediate-to-static-module-signature",
    ":list",
    ":maybe",
    ":numbers",
    ":set",
    ":source-language",
    ":tuples",
    ":types",
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

yaspl_bootstrap_library(
  name = "true_lib",
  srcs = ["true.yaspl"],
  deps = [],
)

yaspl_library(
  name = "hello-world_lib",
  srcs = ["hello-world.yaspl"],
  deps = [],
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

yaspl_bootstrap_library(
  name = "types",
  srcs = ["types.yaspl"],
  deps = [
    ":list",
  ]
)

yaspl_bootstrap_library(
  name = "type-checker",
  srcs = ["type-checker.yaspl"],
  deps = [
    ":intermediate-language",
    ":types",
    ":maybe",
  ]
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


yaspl_library(
  name = "wc_lib",
  srcs = ["wc.yaspl"],
  deps = [
    ":numbers",
  ]
)

yaspl_bootstrap_library(
  name = "x86-64-stack-machine",
  srcs = ["x86-64-stack-machine.yaspl"],
  deps = [
    ":boolean",
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

action_listener(
  name = "yaspl_gen_executables",
  mnemonics = ["YasplLink"],
  extra_actions = [":yaspl_gen_executable"]
)

extra_action(
  name = "yaspl_gen_executable",
  out_templates = ["$(ACTION_ID).executable"],
  tools = [
    ":extra-action-parser",
  ],
  cmd = "$(location :extra-action-parser) $(EXTRA_ACTION_FILE) $(output $(ACTION_ID).executable)"
)
