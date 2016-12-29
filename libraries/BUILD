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
  name = "extra-action-linker",
  main_module = "extra_action_linker_main",
  deps = [":extra-action-linker-main"]
)

yaspl_binary(
  name = "extra-action-linter",
  main_module = "extra_action_linter_main",
  deps = [":extra-action-linter-main"]
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

yaspl_library(
  name = "buffered-port",
  srcs = ["buffered-port.yaspl"],
  deps = [
    "//libraries/data:bytes",
    "//libraries/data:maybe",
    "//libraries/data:tuples"
  ]
)

yaspl_bootstrap_library(
  name = "byte-escapes",
  srcs = ["byte-escapes.yaspl"],
  deps = [
    "//libraries/data:bytes",
    "//libraries/data:either",
  ]
)


yaspl_test(
  name = "byte-escapes-test",
  main_module = "byte_escapes_test",
  srcs = ["byte-escapes-test.yaspl"],
  deps = [
    ":byte-escapes",
    "//libraries/data:bytes",
    "//libraries/data:list",
    ":yunit",
  ],
  size = "small",
)

yaspl_test(
  name = "bytes-test",
  main_module = "bytes_test",
  srcs = ["bytes-test.yaspl"],
  deps = [
    "//libraries/data:boolean",
    "//libraries/data:bytes",
    "//libraries/data:list",
    ":yunit",
  ],
  size = "small",
)

yaspl_bootstrap_library(
  name = "compiler-main",
  srcs = ["compiler-main.yaspl"],
  deps = [
    "//libraries/data:bytes",
    "//libraries/data:dict",
    "//libraries/data:either",
    ":io",
    ":compiler_lib",
    "//libraries/data:list",
    "//libraries/data:maybe",
    ":sexp-parser",
    ":source-language",
    ":intermediate-to-stack",
    ":stack-machine",
    ":stack-machine-optimizer",
    "//libraries/data:tuples",
    ":type-checker",
    ":validator",
    ":x86-64-stack-machine",
 ]
)


yaspl_bootstrap_library(
  name = "environment",
  srcs = ["environment.yaspl"],
  deps = [
    "//libraries/data:dict",
    "//libraries/data:bytes",
    "//libraries/data:list",
    "//libraries/data:maybe",
  ]
)

yaspl_library(
  name = "extra-action-linker-main",
  srcs = ["extra-action-linker-main.yaspl"],
  deps = [
    "//libraries/data:boolean",
    "//libraries/data:bytes",
    "//libraries/data:dict",
    "//libraries/data:either",
    ":extra-action-parser",
    ":io",
    ":linker",
    "//libraries/data:list",
    ":mach-o-writer2_lib",
    ":mach-o_lib",
    "//libraries/data:maybe",
    "//libraries/data:numbers",
    "//libraries/data:set",
  ]
)

yaspl_library(
  name = "extra-action-linter-main",
  srcs = ["extra-action-linter-main.yaspl"],
  deps = [
    "//libraries/data:boolean",
    "//libraries/data:bytes",
    "//libraries/data:dict",
    "//libraries/data:either",
    ":extra-action-parser",
    ":io",
    ":linker",
    "//libraries/data:list",
    ":mach-o-writer2_lib",
    ":mach-o_lib",
    "//libraries/data:maybe",
    "//libraries/data:numbers",
    "//libraries/data:set",
    ":unused-imports",
  ]
)

yaspl_library(
  name = "extra-action-parser",
  srcs = ["extra-action-parser.yaspl"],
  deps = [
    "//libraries/data:boolean",
    "//libraries/data:bytes",
    "//libraries/data:dict",
    "//libraries/data:either",
    ":io",
    ":linker",
    "//libraries/data:list",
    ":mach-o-writer2_lib",
    ":mach-o_lib",
    "//libraries/data:maybe",
    "//libraries/data:numbers",
    "//libraries/data:set",
  ]
)

yaspl_bootstrap_library(
  name = "free-variables",
  srcs = ["free-variables.yaspl"],
  deps = [
    "//libraries/data:bytes",
    "//libraries/data:list",
    "//libraries/data:tuples",
    "//libraries/data:set",
    ":source-language",
  ]
)


yaspl_library(
  name = "code-http-server_lib",
  srcs = ["code-http-server.yaspl"],
  deps = [
    ":buffered-port",
    "//libraries/data:bytes",
    "//libraries/data:either",
    ":http-server_lib",
    ":io",
    ":ip",
    "//libraries/data:join-list",
    "//libraries/data:list",
    ":mach",
    "//libraries/data:maybe",
    "//libraries/data:numbers",
    ":print-stack-function_lib",
    "//libraries/data:regexp",
    ":stack-machine",
    ":tcp",
    "//libraries/data:tuples",
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
    "//libraries/data:tuples",
  ]
)


yaspl_bootstrap_library(
  name = "intermediate-language",
  srcs = ["intermediate-language.yaspl"],
  deps = [
    "//libraries/data:list",
    "//libraries/data:tuples",
    ":source-language",
    ":types",
  ]
)

yaspl_bootstrap_library(
  name = "intermediate-to-static-module-signature",
  srcs = ["intermediate-to-static-module-signature.yaspl"],
  deps = [
    "//libraries/data:bytes",
    "//libraries/data:dict",
    ":intermediate-language",
    "//libraries/data:list",
    ":types",
  ]
)


yaspl_bootstrap_library(
  name = "io",
  srcs = ["io.yaspl"],
  deps = [
    "//libraries/data:bytes"
  ]
)

yaspl_library(
  name = "ip",
  srcs = ["ip.yaspl"],
  deps = [
    "//libraries/data:bytes",
    "//libraries/data:list",
    "//libraries/data:numbers",
    "//libraries/data:regexp",
  ],
)

yaspl_test(
  name = "large-function-test",
  main_module = "large_function_test",
  srcs = ["large-function-test.yaspl"],
  deps = [
    "//libraries/data:list",
    ":yunit",
  ],
  size = "small",
)


yaspl_bootstrap_library(
  name = "lexer",
  srcs = ["lexer.yaspl"],
  deps = [
    "//libraries/data:boolean",
    "//libraries/data:bytes",
    ":byte-escapes",
    ":io",
    "//libraries/data:numbers",
  ]
)

yaspl_test(
  name = "lexer-test",
  main_module = "lexer_test",
  srcs = ["lexer-test.yaspl"],
  deps = [
    "//libraries/data:boolean",
    "//libraries/data:either",
    "//libraries/data:list",
    ":lexer",
    ":yunit",
  ],
  size = "small",
)

yaspl_bootstrap_library(
  name = "library-compiler-main",
  srcs = ["library-compiler-main.yaspl"],
  deps = [
    ":io",
    ":compiler_lib",
    ":sexp-parser",
    ":source-language",
    ":intermediate-to-stack",
    ":module-signature-serialization",
    ":stack-machine-optimizer",
    ":type-checker",
    ":validator",
    ":x86-64-stack-machine",
    ":mach-o-writer2_lib",
    "//libraries/data:bytes",
    "//libraries/data:either",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:tuples",
 ]
)

yaspl_bootstrap_library(
  name = "compiler_lib",
  srcs = ["compiler.yaspl"],
  deps = [
    "//libraries/data:bytes",
    "//libraries/data:dict",
    "//libraries/data:either",
    "//libraries/data:list",
    "//libraries/data:maybe",
    ":prim-implementation",
    ":source-language",
    ":intermediate-to-stack",
    ":stack-machine",
    ":stack-machine-optimizer",
    "//libraries/data:tuples",
    ":validator",
 ]
)

yaspl_library(
  name = "linker",
  srcs = ["linker.yaspl"],
  deps = [
    "//libraries/data:list",
    ":mach-o_lib",
  ]
)

yaspl_library(
  name = "mach",
  srcs = ["mach.yaspl"],
  deps = [
    "//libraries/data:bytes",
    ":io",
    "//libraries/data:list",
    "//libraries/data:numbers",
  ]
)

yaspl_bootstrap_library(
  name = "mach-o_lib",
  srcs = ["mach-o.yaspl"],
  deps = [
    "//libraries/data:boolean",
    "//libraries/data:bytes",
    "//libraries/data:either",
    ":io",
    "//libraries/data:numbers",
  ]
)

yaspl_bootstrap_library(
  name = "mach-o-writer_lib",
  srcs = ["mach-o-writer.yaspl"],
  deps = [
    "//libraries/data:bytes",
    "//libraries/data:dict",
    ":io",
    "//libraries/data:list",
    "//libraries/data:numbers",
  ]
)

yaspl_bootstrap_library(
  name = "mach-o-writer2_lib",
  srcs = ["mach-o-writer2.yaspl"],
  deps = [
    ":io",
    "//libraries/data:bytes",
    "//libraries/data:list",
    ":x86-64-stack-machine",
    "//libraries/data:dict",
    ":prim-implementation",
    ":source-language",
    ":sexp-parser",
    "//libraries/data:either",
    ":mach-o-writer_lib",
    ":mach-o_lib",
  ]
)

yaspl_bootstrap_library(
  name = "main-stub_lib",
  srcs = ["main-stub.yaspl"],
  deps = [
    "//libraries/data:bytes",
    ":io",
    "//libraries/data:list",
    ":stack-machine-optimizer",
    ":x86-64-stack-machine",
    ":mach-o-writer2_lib",
  ]
)

yaspl_bootstrap_library(
  name = "module-signature",
  srcs = ["module-signature.yaspl"],
  deps = [
    "//libraries/data:dict",
  ]
)

yaspl_bootstrap_library(
  name = "module-signature-serialization",
  srcs = ["module-signature-serialization.yaspl"],
  deps = [
    ":module-signature",
    ":sexp-parser",
    ":sexp-printer",
    "//libraries/data:dict",
    "//libraries/data:either",
  ]
)

yaspl_test(
  name = "module-signature-serialization-test",
  main_module = "module_signature_serialization_test",
  srcs = ["module-signature-serialization-test.yaspl"],
  deps = [
    ":module-signature",
    ":module-signature-serialization",
    ":yunit",
    "//libraries/data:list",
  ]
)


yaspl_bootstrap_library(
  name = "prim-implementation",
  srcs = ["prim-implementation.yaspl"],
  deps = [
    "//libraries/data:dict",
    "//libraries/data:bytes",
    ":module-signature",
    ":intermediate-to-static-module-signature",
    "//libraries/data:list",
    ":stack-machine",
    "//libraries/data:tuples",
    "//libraries/data:set",
  ]
)

yaspl_library(
  name = "print-stack-function_lib",
  srcs = ["print-stack-function.yaspl"],
  deps = [
    "//libraries/data:bytes",
    "//libraries/data:either",
    ":io",
    "//libraries/data:list",
    ":compiler_lib",
    ":sexp-parser",
    ":source-language",
    ":intermediate-to-stack",
    ":stack-machine",
    "//libraries/data:tuples",
 ]
)

yaspl_bootstrap_library(
  name = "stack-machine-optimizer",
  srcs = ["stack-machine-optimizer.yaspl"],
  deps = [
    "//libraries/data:bytes",
    "//libraries/data:dict",
    "//libraries/data:either",
    ":io",
    "//libraries/data:list",
    ":prim-implementation",
    ":sexp-parser",
    ":source-language",
    ":intermediate-to-stack",
    ":stack-machine",
    "//libraries/data:tuples",
 ]
)

yaspl_bootstrap_library(
  name = "sexp-parser",
  srcs = ["sexp-parser.yaspl"],
  deps = [
    "//libraries/data:either",
    ":io",
    ":lexer",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:numbers",
  ]
)

yaspl_test(
  name = "sexp-parser-test",
  main_module = "sexp_parser_test",
  srcs = ["sexp-parser-test.yaspl"],
  deps = [
    "//libraries/data:boolean",
    "//libraries/data:either",
    "//libraries/data:list",
    ":sexp-parser",
    ":yunit",
  ],
  size = "small",
)

yaspl_bootstrap_library(
  name = "sexp-printer",
  srcs = ["sexp-printer.yaspl"],
  deps = [
    ":sexp-parser",
    "//libraries/data:join-list",
  ]
)

yaspl_test(
  name = "sexp-printer-test",
  main_module = "sexp_printer_test",
  srcs = ["sexp-printer-test.yaspl"],
  deps = [
    "//libraries/data:bytes",
    "//libraries/data:either",
    "//libraries/data:list",
    ":sexp-parser",
    ":sexp-printer",
    ":yunit",
  ],
  size = "small",
)


yaspl_bootstrap_library(
  name = "source-language",
  srcs = ["source-language.yaspl"],
  deps = [
   "//libraries/data:either",
   ":io",
   "//libraries/data:list",
   "//libraries/data:maybe",
   ":sexp-parser",
   "//libraries/data:tuples",
  ]
)

yaspl_bootstrap_library(
  name = "source-to-intermediate-language",
  srcs = ["source-to-intermediate-language.yaspl"],
  deps = [
    "//libraries/data:bytes",
    "//libraries/data:dict",
    ":free-variables",
    ":intermediate-language",
    ":intermediate-to-static-module-signature",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:numbers",
    "//libraries/data:set",
    ":source-language",
    "//libraries/data:tuples",
    ":types",
  ]
)


yaspl_bootstrap_library(
  name = "intermediate-to-module-signature",
  srcs = ["intermediate-to-module-signature.yaspl"],
  deps = [
    ":environment",
    ":module-signature",
    ":source-language",
    ":source-to-intermediate-language",
    ":stack-machine",
    "//libraries/data:bytes",
    "//libraries/data:dict",
    "//libraries/data:either",
    "//libraries/data:join-list",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:tuples",
  ]
)


yaspl_bootstrap_library(
  name = "intermediate-to-stack",
  srcs = ["intermediate-to-stack.yaspl"],
  deps = [
    ":environment",
    ":intermediate-to-module-signature",
    ":module-signature",
    ":source-language",
    ":source-to-intermediate-language",
    ":stack-machine",
    "//libraries/data:bytes",
    "//libraries/data:dict",
    "//libraries/data:either",
    "//libraries/data:join-list",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:tuples",
  ]
)

yaspl_bootstrap_library(
  name = "stack-machine",
  srcs = ["stack-machine.yaspl"],
  deps = [
    "//libraries/data:bytes",
    ":io",
    "//libraries/data:join-list",
    "//libraries/data:list",
    "//libraries/data:numbers",
    ":sexp-parser",
    "//libraries/data:tuples",
  ],
)


yaspl_test(
  name = "tail-call-test",
  main_module = "tail_call_test",
  srcs = ["tail-call-test.yaspl"],
  deps = [
    "//libraries/data:list",
    ":yunit",
  ],
  size = "small",
)

yaspl_library(
  name = "tcp",
  srcs = ["tcp.yaspl"],
  deps = [
    ":ip",
    "//libraries/data:tuples",
  ]
)

yaspl_bootstrap_library(
  name = "types",
  srcs = ["types.yaspl"],
  deps = [
    "//libraries/data:list",
  ]
)

yaspl_bootstrap_library(
  name = "type-checker",
  srcs = ["type-checker.yaspl"],
  deps = [
    ":intermediate-language",
    ":types",
    "//libraries/data:list",
    "//libraries/data:dict",
    "//libraries/data:maybe",
  ]
)



yaspl_library(
  name = "udp",
  srcs = ["udp.yaspl"],
  deps = [
    ":ip",
    "//libraries/data:tuples",
  ]
)

yaspl_library(
  name = "unused-imports",
  srcs = ["unused-imports.yaspl"],
  deps = [
    ":source-language",
    "//libraries/data:list",
    ":validator",
    "//libraries/data:set",
    "//libraries/data:multi-set",
    "//libraries/data:bytes",
  ]
)


yaspl_bootstrap_library(
  name = "validator",
  srcs = ["validator.yaspl"],
  deps = [
    ":source-language",
    ":module-signature",
    "//libraries/data:bytes",
    "//libraries/data:dict",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:multi-set",
    "//libraries/data:set",
    "//libraries/data:tuples",
  ]
)


yaspl_bootstrap_library(
  name = "x86-64-stack-machine",
  srcs = ["x86-64-stack-machine.yaspl"],
  deps = [
    "//libraries/data:boolean",
    "//libraries/data:bytes",
    "//libraries/data:either",
    ":io",
    "//libraries/data:list",
    "//libraries/data:numbers",
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
    "//libraries/data:list",
  ]
)

action_listener(
  name = "yaspl_gen_executables",
  mnemonics = ["YasplLink"],
  extra_actions = [":yaspl_gen_executable"]
)

action_listener(
  name = "yaspl-lint",
  mnemonics = ["YasplCompile"],
  extra_actions = [":yaspl-lint-action"]
)


extra_action(
  name = "yaspl_gen_executable",
  out_templates = ["$(ACTION_ID).executable"],
  tools = [
    ":extra-action-linker",
  ],
  cmd = "$(location :extra-action-linker) $(EXTRA_ACTION_FILE) $(output $(ACTION_ID).executable)"
)

extra_action(
  name = "yaspl-lint-action",
  out_templates = ["$(ACTION_ID).lint"],
  tools = [
    ":extra-action-linter",
  ],
  cmd = "$(location :extra-action-linter) $(EXTRA_ACTION_FILE) $(output $(ACTION_ID).lint)"
)
