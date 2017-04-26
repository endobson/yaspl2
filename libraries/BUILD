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

filegroup(
  name = "linker_library_files",
  visibility = ["//visibility:public"],
  data = [":linker-main.srcs", ":linker-main.src.list"]
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
  name = "linker",
  main_module = "linker_main",
  deps = [":linker-main"]
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
    ":yunit",
    "//libraries/data:bytes",
    "//libraries/data:either",
    "//libraries/data:list",
  ],
  size = "small",
)

yaspl_bootstrap_library(
  name = "compiler-main",
  srcs = ["compiler-main.yaspl"],
  deps = [
    ":compiler_lib",
    ":io",
    ":x86-64-stack-machine",
    "//libraries/yaspl:x86-64-runtime",
    "//libraries/data:bytes",
    "//libraries/data:either",
    "//libraries/data:list",
 ]
)


yaspl_bootstrap_library(
  name = "environment",
  srcs = ["environment.yaspl"],
  deps = [
    ":types",
    "//libraries/data:dict",
    "//libraries/data:bytes",
    "//libraries/data:list",
    "//libraries/data:maybe",
  ]
)

yaspl_bootstrap_library(
  name = "linker-main",
  srcs = ["linker-main.yaspl"],
  deps = [
    ":io",
    ":linker_lib",
    ":mach-o-writer2_lib",
    ":mach-o_lib",
    "//libraries/data:bytes",
    "//libraries/data:either",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:set",
  ]
)

yaspl_library(
  name = "extra-action-linter-main",
  srcs = ["extra-action-linter-main.yaspl"],
  deps = [
    ":extra-action-parser",
    ":io",
    "//libraries/data:boolean",
    "//libraries/data:bytes",
    "//libraries/data:either",
    "//libraries/data:list",
    "//tools:linter_lib",
  ]
)

yaspl_library(
  name = "module-header-lint",
  srcs = ["module-header-lint.yaspl"],
  deps = [
    ":source-language",
    "//libraries/data:bytes",
    "//libraries/data:list"
  ]
)


yaspl_library(
  name = "extra-action-parser",
  srcs = ["extra-action-parser.yaspl"],
  deps = [
    ":io",
    "//libraries/data:bytes",
    "//libraries/data:either",
    "//libraries/data:list",
  ]
)

yaspl_bootstrap_library(
  name = "free-variables",
  srcs = ["free-variables.yaspl"],
  deps = [
    ":source-language",
    "//libraries/data:bytes",
    "//libraries/data:maybe",
    "//libraries/data:list",
    "//libraries/data:tuples",
    "//libraries/data:set",
  ]
)


yaspl_library(
  name = "code-http-server_lib",
  srcs = ["code-http-server.yaspl"],
  deps = [
    ":http-server_lib",
    ":io",
    ":ip",
    ":print-stack-function_lib",
    ":stack-machine",
    ":svg",
    ":tcp",
    ":xml",
    ":x86-64-stack-machine",
    "//libraries/algorithms:depth-first-search",
    "//libraries/data:bytes",
    "//libraries/data:either",
    "//libraries/data:dict",
    "//libraries/data:join-list",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:numbers",
    "//libraries/data:tuples",
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
    "//libraries/data:bytes",
    "//libraries/data:tuples",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:numbers",
    "//libraries/data:regexp",
  ]
)


yaspl_bootstrap_library(
  name = "intermediate-language",
  srcs = ["intermediate-language.yaspl"],
  deps = [
    ":source-language",
    ":types",
    "//libraries/data:list",
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
    "//libraries/data:maybe",
    "//libraries/data:numbers",
    "//libraries/data:regexp",
  ],
)

yaspl_library(
  name = "graphviz",
  srcs = ["graphviz.yaspl"],
  deps = [
    "//libraries/data:undirected-graph",
    "//libraries/data:bytes",
    "//libraries/data:dict",
    "//libraries/data:join-list",
    "//libraries/data:maybe",
    "//libraries/data:tuples",
  ]
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
    ":byte-escapes",
    ":io",
    "//libraries/data:boolean",
    "//libraries/data:bytes",
    "//libraries/data:either",
    "//libraries/data:numbers",
  ]
)

yaspl_test(
  name = "lexer-test",
  main_module = "lexer_test",
  srcs = ["lexer-test.yaspl"],
  deps = [
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
    ":mach-o-writer2_lib",
    ":module-signature",
    ":module-signature-serialization",
    ":x86-64-stack-machine",
    "//libraries/data:either",
    "//libraries/data:list",
    "//libraries/data:tuples",
 ]
)

yaspl_bootstrap_library(
  name = "compiler_lib",
  srcs = ["compiler.yaspl"],
  deps = [
    ":intermediate-to-stack",
    ":io",
    ":module-signature",
    ":prim-implementation",
    ":sexp-parser",
    ":source-language",
    ":source-to-intermediate-language",
    ":stack-machine",
    ":stack-machine-optimizer",
    ":validator",
    "//libraries/data:bytes",
    "//libraries/data:dict",
    "//libraries/data:either",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:tuples",
 ]
)

yaspl_bootstrap_library(
  name = "linker_lib",
  srcs = ["linker.yaspl"],
  deps = [
    ":mach-o_lib",
    "//libraries/data:boolean",
    "//libraries/data:bytes",
    "//libraries/data:list",
    "//libraries/data:maybe",
  ]
)

yaspl_library(
  name = "mach",
  srcs = ["mach.yaspl"],
  deps = [
    "//libraries/data:bytes",
    "//libraries/data:list",
  ]
)

yaspl_bootstrap_library(
  name = "mach-o_lib",
  srcs = ["mach-o.yaspl"],
  deps = [
    ":io",
    "//libraries/data:boolean",
    "//libraries/data:bytes",
    "//libraries/data:either",
    "//libraries/data:list",
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
  ]
)

yaspl_bootstrap_library(
  name = "mach-o-writer2_lib",
  srcs = ["mach-o-writer2.yaspl"],
  deps = [
    ":io",
    ":x86-64-stack-machine",
    ":mach-o-writer_lib",
    ":mach-o_lib",
    "//libraries/data:bytes",
    "//libraries/data:either",
    "//libraries/data:join-list",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:set",
    "//libraries/data:tuples",
  ]
)

yaspl_bootstrap_library(
  name = "main-stub_lib",
  srcs = ["main-stub.yaspl"],
  deps = [
    ":io",
    ":stack-machine-optimizer",
    ":mach-o-writer2_lib",
    ":stack-machine",
    ":prim-implementation",
    ":x86-64-stack-machine",
    "//libraries/yaspl:x86-64-runtime",
    "//libraries/data:bytes",
    "//libraries/data:dict",
    "//libraries/data:either",
    "//libraries/data:list",
  ]
)

yaspl_bootstrap_library(
  name = "module-signature",
  srcs = ["module-signature.yaspl"],
  deps = [
    ":types",
    "//libraries/data:dict",
    "//libraries/data:list",
  ]
)

yaspl_bootstrap_library(
  name = "module-signature-serialization",
  srcs = ["module-signature-serialization.yaspl"],
  deps = [
    ":module-signature",
    ":sexp-parser",
    ":sexp-printer",
    ":types",
    "//libraries/data:bytes",
    "//libraries/data:dict",
    "//libraries/data:either",
    "//libraries/data:list",
    "//libraries/data:maybe",
  ]
)

yaspl_test(
  name = "module-signature-serialization-test",
  main_module = "module_signature_serialization_test",
  srcs = ["module-signature-serialization-test.yaspl"],
  deps = [
    ":module-signature",
    ":module-signature-serialization",
    ":types",
    ":yunit",
    "//libraries/data:bytes",
    "//libraries/data:dict",
    "//libraries/data:either",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:tuples",
  ]
)


yaspl_bootstrap_library(
  name = "prim-implementation",
  srcs = ["prim-implementation.yaspl"],
  deps = [
    ":module-signature",
    ":stack-machine",
    ":types",
    "//libraries/data:dict",
    "//libraries/data:bytes",
    "//libraries/data:list",
    "//libraries/data:tuples",
    "//libraries/data:set",
  ]
)

yaspl_library(
  name = "print-stack-function_lib",
  srcs = ["print-stack-function.yaspl"],
  deps = [
    ":compiler_lib",
    ":io",
    ":stack-machine",
    "//libraries/data:bytes",
    "//libraries/data:either",
    "//libraries/data:join-list",
    "//libraries/data:list",
    "//libraries/data:numbers",
 ]
)

yaspl_bootstrap_library(
  name = "stack-machine-optimizer",
  srcs = ["stack-machine-optimizer.yaspl"],
  deps = [
    ":stack-machine",
    "//libraries/data:dict",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:set",
    "//libraries/data:tuples",
 ]
)

yaspl_bootstrap_library(
  name = "sexp-parser",
  srcs = ["sexp-parser.yaspl"],
  deps = [
    ":lexer",
    "//libraries/data:bytes",
    "//libraries/data:either",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:numbers",
    "//libraries/data:tuples",
  ]
)

yaspl_test(
  name = "sexp-parser-test",
  main_module = "sexp_parser_test",
  srcs = ["sexp-parser-test.yaspl"],
  deps = [
    ":sexp-parser",
    ":yunit",
    "//libraries/data:bytes",
    "//libraries/data:either",
    "//libraries/data:list",
  ],
  size = "small",
)

yaspl_bootstrap_library(
  name = "sexp-printer",
  srcs = ["sexp-printer.yaspl"],
  deps = [
    ":sexp-parser",
    "//libraries/data:bytes",
    "//libraries/data:join-list",
    "//libraries/data:list",
    "//libraries/data:numbers",
  ]
)

yaspl_test(
  name = "sexp-printer-test",
  main_module = "sexp_printer_test",
  srcs = ["sexp-printer-test.yaspl"],
  deps = [
    ":sexp-parser",
    ":sexp-printer",
    ":yunit",
    "//libraries/data:bytes",
    "//libraries/data:either",
    "//libraries/data:list",
  ],
  size = "small",
)


yaspl_bootstrap_library(
  name = "source-language",
  srcs = ["source-language.yaspl"],
  deps = [
   ":sexp-parser",
   "//libraries/data:either",
   "//libraries/data:list",
   "//libraries/data:maybe",
   "//libraries/data:tuples",
  ]
)

yaspl_bootstrap_library(
  name = "source-to-intermediate-language",
  srcs = ["source-to-intermediate-language.yaspl"],
  deps = [
    ":free-variables",
    ":intermediate-language",
    ":module-signature",
    ":source-language",
    ":types",
    ":type-checker",
    ":type-checker-context",
    "//libraries/data:bytes",
    "//libraries/data:dict",
    "//libraries/data:either",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:numbers",
    "//libraries/data:set",
    "//libraries/data:tuples",
  ]
)


yaspl_bootstrap_library(
  name = "intermediate-to-module-signature",
  srcs = ["intermediate-to-module-signature.yaspl"],
  deps = [
    ":environment",
    ":module-signature",
    ":types",
    ":intermediate-language",
    "//libraries/data:bytes",
    "//libraries/data:dict",
    "//libraries/data:list",
    "//libraries/data:maybe",
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
    ":stack-machine",
    ":types",
    ":intermediate-language",
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
    "//libraries/data:join-list",
    "//libraries/data:list",
    "//libraries/data:tuples",
  ],
)

yaspl_library(
  name = "svg",
  srcs = ["svg.yaspl"],
  deps = [
    ":xml",
    "//libraries/data:join-list",
    "//libraries/data:list",
    "//libraries/data:numbers",
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
  name = "totality-checker",
  srcs = ["totality-checker.yaspl"],
  deps = [
    ":source-language",
    ":type-checker-context",
    "//libraries/data:bytes",
    "//libraries/data:either",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:tuples",
  ]
)

yaspl_bootstrap_library(
  name = "types",
  srcs = ["types.yaspl"],
  deps = [
    ":sexp-parser",
    ":sexp-printer",
    "//libraries/data:bytes",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:tuples",
  ]
)

yaspl_test(
  name = "types-test",
  main_module = "types_test",
  srcs = ["types-test.yaspl"],
  deps = [
    ":types",
    ":yunit",
    "//libraries/data:bytes",
    "//libraries/data:list",
    "//libraries/data:maybe",
  ]
)


yaspl_bootstrap_library(
  name = "type-checker",
  srcs = ["type-checker.yaspl"],
  deps = [
    ":module-signature",
    ":sexp-printer",
    ":source-language",
    ":types",
    ":type-checker-context",
    ":type-unification",
    ":totality-checker",
    ":prim-implementation",
    "//libraries/data:bytes",
    "//libraries/data:dict",
    "//libraries/data:either",
    "//libraries/data:join-list",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:numbers",
    "//libraries/data:set",
    "//libraries/data:tuples",
  ]
)

yaspl_bootstrap_library(
  name = "type-checker-context",
  srcs = ["type-checker-context.yaspl"],
  deps = [
    ":types",
    "//libraries/data:bytes",
    "//libraries/data:dict",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:tuples",
  ]
)


yaspl_test(
  name = "type-checker-test",
  main_module = "type_checker_test",
  srcs = ["type-checker-test.yaspl"],
  deps = [
    ":source-language",
    ":types",
    ":type-checker",
    ":type-checker-context",
    ":yunit",
    "//libraries/data:bytes",
    "//libraries/data:dict",
    "//libraries/data:either",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:tuples",
  ]
)

yaspl_bootstrap_library(
  name = "type-unification",
  srcs = ["type-unification.yaspl"],
  deps = [
    ":types",
    ":sexp-parser",
    ":sexp-printer",
    "//libraries/data:bytes",
    "//libraries/data:dict",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:numbers",
    "//libraries/data:tuples",
  ]
)

yaspl_test(
  name = "type-unification-test",
  main_module = "type_unification_test",
  srcs = ["type-unification-test.yaspl"],
  deps = [
    ":types",
    ":type-unification",
    ":yunit",
    "//libraries/data:bytes",
    "//libraries/data:dict",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:tuples",
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
  name = "unused-bindings",
  srcs = ["unused-bindings.yaspl"],
  deps = [
    ":source-language",
    ":validator",
    ":free-variables",
    "//libraries/data:bytes",
    "//libraries/data:list",
    "//libraries/data:multi-set",
    "//libraries/data:set",
  ]
)


yaspl_bootstrap_library(
  name = "validator",
  srcs = ["validator.yaspl"],
  deps = [
    ":module-signature",
    ":source-language",
    ":free-variables",
    "//libraries/data:bytes",
    "//libraries/data:dict",
    "//libraries/data:either",
    "//libraries/data:join-list",
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
    ":io",
    ":stack-machine",
    "//libraries/data:bytes",
    "//libraries/data:join-list",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:numbers",
  ]
)

yaspl_library(
  name = "xml",
  srcs = ["xml.yaspl"],
  deps = [
    "//libraries/data:list",
    "//libraries/data:bytes",
    "//libraries/data:join-list",
  ]
)

yaspl_library(
  name = "yunit",
  srcs = ["yunit.yaspl"],
  deps = [
    ":io",
    "//libraries/data:dict",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:tuples",
  ]
)

action_listener(
  name = "yaspl-lint",
  mnemonics = ["YasplCompile"],
  extra_actions = [":yaspl-lint-action"]
)

extra_action(
  name = "yaspl-lint-action",
  out_templates = ["$(ACTION_ID).lint"],
  tools = [
    ":extra-action-linter",
  ],
  cmd = "$(location :extra-action-linter) $(EXTRA_ACTION_FILE) $(output $(ACTION_ID).lint)"
)
