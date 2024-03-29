load(
    ":yaspl.bzl",
    "yaspl_bootstrap_binary",
    "yaspl_bootstrap_library",
    "yaspl_library",
    "yaspl_test",
)

package(
    default_visibility = ["//visibility:public"],
)

yaspl_bootstrap_binary(
    name = "compiler",
    srcs = ["compiler-main.yaspl"],
    deps = [
        ":compiler_lib",
        ":io",
        ":linker_lib",
        ":machine-code-writer",
        ":module-name",
        ":system-abi",
        ":top-level-name",
        "//libraries/core-language:compiler-lib",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
        "//libraries/prim-language:prim-language-compiler_lib",
        "//libraries/yaspl:parser",
        "//libraries/yaspl:top-level-objects-to-x86-64",
        "//libraries/yaspl:x86-64-assembly",
        "//libraries/yaspl:x86-64-runtime",
        "//libraries/yaspl/frontend:errors",
    ],
)

yaspl_bootstrap_binary(
    name = "library-compiler",
    srcs = ["library-compiler-main.yaspl"],
    deps = [
        ":compiler_lib",
        ":io",
        ":machine-code-writer",
        ":system-abi",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/yaspl:parser",
        "//libraries/yaspl/frontend:errors",
    ],
)

yaspl_bootstrap_binary(
    name = "linker",
    srcs = ["linker-main.yaspl"],
    deps = [
        ":io",
        ":linker_lib",
        ":machine-code-writer",
        "//libraries/data:bytes",
        "//libraries/data:list",
        "//libraries/data:maybe",
    ],
)

yaspl_bootstrap_binary(
    name = "main-stub",
    srcs = ["main-stub-main.yaspl"],
    deps = [
        ":io",
        ":machine-code-writer",
        ":module-signature",
        ":module-signature-serialization",
        ":prim-implementation",
        ":prim-types",
        ":system-abi",
        ":types",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/yaspl:top-level-object-optimizer",
        "//libraries/yaspl:top-level-objects-to-x86-64",
        "//libraries/yaspl:x86-64-assembly",
        "//libraries/yaspl:x86-64-runtime",
    ],
)

yaspl_bootstrap_library(
    name = "assembler",
    srcs = ["assembler.yaspl"],
    deps = [
        ":panic",
        ":structured-native-code",
        "//libraries/data:aligned-bytes",
        "//libraries/data:bytes",
        "//libraries/data:join-list",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:set",
        "//libraries/data:tuples",
        "//libraries/yaspl:x86-64-assembly",
        "//libraries/yaspl:x86-64-instruction-assembler",
    ],
)

yaspl_library(
    name = "buffered-port",
    srcs = ["buffered-port.yaspl"],
    deps = [
        "//libraries/data:bytes",
        "//libraries/data:lifted-primitives",
        "//libraries/data:maybe",
    ],
)

filegroup(
    name = "buffered-port-test-data",
    srcs = glob(["test-data/buffered-port-test/*"]),
)

yaspl_test(
    name = "buffered-port-test",
    size = "small",
    srcs = ["buffered-port-test.yaspl"],
    data = [
        ":buffered-port-test-data",
    ],
    deps = [
        ":buffered-port",
        ":io",
        ":yunit",
        "//libraries/data:bytes",
        "//libraries/data:list",
        "//libraries/data:maybe",
    ],
)

yaspl_bootstrap_library(
    name = "byte-escapes",
    srcs = ["byte-escapes.yaspl"],
    deps = [
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
    ],
)

yaspl_test(
    name = "byte-escapes-test",
    size = "small",
    srcs = ["byte-escapes-test.yaspl"],
    deps = [
        ":byte-escapes",
        ":yunit",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
    ],
)

yaspl_library(
    name = "stack-trace",
    srcs = ["stack-trace.yaspl"],
    deps = [
        ":in-memory-mach-o",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:ordering",
        "//libraries/data:red-black-tree",
        "//libraries/data:tuples",
        "//libraries/formats:mach-o",
        "//libraries/yaspl/runtime:fill-stack",
    ],
)

yaspl_library(
    name = "module-header-lint",
    srcs = ["module-header-lint.yaspl"],
    deps = [
        ":module-name",
        "//libraries/data:bytes",
        "//libraries/data:list",
        "//libraries/yaspl/languages:source-language",
    ],
)

yaspl_library(
    name = "extra-action-parser",
    srcs = ["extra-action-parser.yaspl"],
    deps = [
        ":io",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
    ],
)

yaspl_bootstrap_library(
    name = "free-variables",
    srcs = ["free-variables.yaspl"],
    deps = [
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/yaspl/languages:source-language",
    ],
)

yaspl_library(
    name = "http-server",
    srcs = ["http-server.yaspl"],
    deps = [
        ":buffered-port",
        ":io",
        ":tcp",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:regexp",
        "//libraries/data:tuples",
        "//libraries/protocols/http:http-response",
    ],
)

yaspl_bootstrap_library(
    name = "intermediate-expression",
    srcs = ["intermediate-expression.yaspl"],
    deps = [
        ":top-level-name",
        ":types",
        ":variant-info",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/yaspl:math-operations",
        "//libraries/yaspl:var",
    ],
)

yaspl_bootstrap_library(
    name = "intermediate-expression-printer",
    srcs = ["intermediate-expression-printer.yaspl"],
    deps = [
        ":intermediate-expression",
        ":pretty-print",
        "//libraries/data:list",
        "//libraries/data:numbers",
        "//libraries/yaspl:var",
    ],
)

yaspl_bootstrap_library(
    name = "intermediate-language",
    srcs = ["intermediate-language.yaspl"],
    deps = [
        ":intermediate-expression",
        ":module-name",
        ":module-signature",
        ":top-level-name",
        ":types",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/yaspl:var",
    ],
)

yaspl_bootstrap_library(
    name = "io",
    srcs = ["io.yaspl"],
    deps = [
        ":panic",
        "//libraries/data:bytes",
        "//libraries/data:source-location",
    ],
)

yaspl_library(
    name = "ip",
    srcs = ["ip.yaspl"],
    deps = [
        "//libraries/data:bytes",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:regexp",
    ],
)

yaspl_bootstrap_library(
    name = "graphviz",
    srcs = ["graphviz.yaspl"],
    deps = [
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:directed-graph",
        "//libraries/data:join-list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
        "//libraries/data:undirected-graph",
    ],
)

yaspl_test(
    name = "large-function-test",
    size = "small",
    srcs = ["large-function-test.yaspl"],
    deps = [
        ":yunit",
        "//libraries/data:list",
    ],
)

yaspl_bootstrap_library(
    name = "lexer",
    srcs = ["lexer.yaspl"],
    deps = [
        "//libraries/data:bytes",
        "//libraries/data:maybe",
        "//libraries/data:range-map",
        "//libraries/data:source-location",
    ],
)

yaspl_bootstrap_library(
    name = "system-abi",
    srcs = ["system-abi.yaspl"],
)

yaspl_bootstrap_library(
    name = "machine-code-writer",
    srcs = ["machine-code-writer.yaspl"],
    deps = [
        ":assembler",
        ":io",
        ":module-signature-serialization",
        ":system-abi",
        "//libraries/data:maybe",
        "//libraries/formats:elf-writer",
        "//libraries/formats:mach-o-writer",
        "//libraries/formats:pe-writer",
        "//libraries/yaspl:top-level-objects",
        "//libraries/yaspl:top-level-objects-to-x86-64",
        "//libraries/yaspl:x86-64-assembly",
    ],
)

yaspl_bootstrap_library(
    name = "compiler_lib",
    srcs = ["compiler.yaspl"],
    deps = [
        ":intermediate-language",
        ":intermediate-to-module-signature",
        ":module-name",
        ":module-signature",
        ":prim-implementation",
        ":source-to-intermediate-language",
        ":validator",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:indexed-set",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
        "//libraries/yaspl:intermediate-to-register",
        "//libraries/yaspl:optimize-intermediate",
        "//libraries/yaspl:top-level-object-optimizer",
        "//libraries/yaspl:top-level-objects",
        "//libraries/yaspl/conversions:resolve-imports",
        "//libraries/yaspl/conversions:resolve-types",
        "//libraries/yaspl/frontend:errors",
        "//libraries/yaspl/languages:source-language",
    ],
)

yaspl_bootstrap_library(
    name = "linker_lib",
    srcs = ["linker.yaspl"],
    deps = [
        ":io",
        ":structured-elf",
        ":structured-mach-o",
        ":structured-native-code",
        ":structured-pe",
        ":system-abi",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/formats:elf",
        "//libraries/formats:elf-writer",
        "//libraries/formats:mach-o",
        "//libraries/formats:mach-o-writer",
        "//libraries/formats:pe",
        "//libraries/formats:pe-writer",
    ],
)

yaspl_library(
    name = "mach",
    srcs = ["mach.yaspl"],
    deps = [
        ":panic",
        "//libraries/data:bytes",
        "//libraries/data:list",
    ],
)

yaspl_library(
    name = "in-memory-mach-o",
    srcs = ["in-memory-mach-o.yaspl"],
    deps = [
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:tuples",
        "//libraries/formats:mach-o",
    ],
)

yaspl_bootstrap_library(
    name = "structured-elf",
    srcs = ["structured-elf.yaspl"],
    deps = [
        ":structured-native-code",
        "//libraries/data:aligned-bytes",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:tuples",
        "//libraries/formats:elf",
    ],
)

yaspl_bootstrap_library(
    name = "structured-mach-o",
    srcs = ["structured-mach-o.yaspl"],
    deps = [
        ":structured-native-code",
        "//libraries/data:aligned-bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/formats:mach-o",
    ],
)

yaspl_bootstrap_library(
    name = "structured-native-code",
    srcs = ["structured-native-code.yaspl"],
    deps = [
        "//libraries/data:aligned-bytes",
        "//libraries/data:bytes",
        "//libraries/data:list",
        "//libraries/data:numbers",
    ],
)

yaspl_bootstrap_library(
    name = "structured-pe",
    srcs = ["structured-pe.yaspl"],
    deps = [
        ":structured-native-code",
        "//libraries/data:aligned-bytes",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:list-multi-dict",
        "//libraries/data:maybe",
        "//libraries/formats:pe",
    ],
)

yaspl_bootstrap_library(
    name = "module-name",
    srcs = ["module-name.yaspl"],
    deps = [
        ":sexp-printer",
        "//libraries/data:bytes",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:ordering",
        "//libraries/formats:sexp",
    ],
)

yaspl_bootstrap_library(
    name = "module-signature",
    srcs = ["module-signature.yaspl"],
    deps = [
        ":intermediate-expression",
        ":module-name",
        ":top-level-name",
        ":types",
        ":variant-info",
        "//libraries/core-language:struct-info",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:maybe",
    ],
)

yaspl_bootstrap_library(
    name = "module-signature-serialization",
    srcs = ["module-signature-serialization.yaspl"],
    deps = [
        ":module-name",
        ":module-signature",
        ":sexp-parser",
        ":sexp-printer",
        ":top-level-name",
        ":types",
        ":variant-info",
        "//libraries/core-language:struct-info",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
        "//libraries/formats:sexp",
    ],
)

yaspl_test(
    name = "module-signature-serialization-test",
    srcs = ["module-signature-serialization-test.yaspl"],
    deps = [
        ":module-name",
        ":module-signature",
        ":module-signature-serialization",
        ":panic",
        ":top-level-name",
        ":types",
        ":variant-info",
        ":yunit",
        "//libraries/core-language:struct-info",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
    ],
)

yaspl_bootstrap_library(
    name = "panic",
    srcs = ["panic.yaspl"],
    deps = [],
)

yaspl_bootstrap_library(
    name = "prim-implementation",
    srcs = ["prim-implementation.yaspl"],
    deps = [
        ":intermediate-expression",
        ":intermediate-language",
        ":module-signature",
        ":prim-names",
        ":prim-types",
        ":top-level-name",
        ":types",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
        "//libraries/yaspl:intermediate-to-register",
        "//libraries/yaspl:math-operations",
        "//libraries/yaspl:top-level-objects",
        "//libraries/yaspl:var",
    ],
)

yaspl_bootstrap_library(
    name = "prim-names",
    srcs = ["prim-names.yaspl"],
    deps = [
        ":module-name",
        ":top-level-name",
        "//libraries/data:list",
        "//libraries/yaspl:mangle-names",
    ],
)

yaspl_bootstrap_library(
    name = "prim-types",
    srcs = ["prim-types.yaspl"],
    deps = [
        ":module-name",
        ":prim-names",
        ":types",
        "//libraries/data:list",
        "//libraries/data:maybe",
    ],
)

yaspl_bootstrap_library(
    name = "pretty-print",
    srcs = ["pretty-print.yaspl"],
    deps = [
        "//libraries/data:join-list",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
    ],
)

yaspl_test(
    name = "pretty-print-test",
    srcs = ["pretty-print-test.yaspl"],
    deps = [
        ":pretty-print",
        ":yunit",
        "//libraries/data:bytes",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:maybe",
    ],
)

yaspl_bootstrap_library(
    name = "sexp-lexer",
    srcs = ["sexp-lexer.yaspl"],
    deps = [
        ":byte-escapes",
        ":lexer",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:numbers",
        "//libraries/data:source-location",
    ],
)

yaspl_test(
    name = "sexp-lexer-test",
    size = "small",
    srcs = ["sexp-lexer-test.yaspl"],
    deps = [
        ":sexp-lexer",
        ":yunit",
        "//libraries/data:bytes",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
    ],
)

yaspl_bootstrap_library(
    name = "sexp-parser",
    srcs = ["sexp-parser.yaspl"],
    deps = [
        ":sexp-lexer",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:source-location",
        "//libraries/formats:sexp",
    ],
)

yaspl_test(
    name = "sexp-parser-test",
    size = "small",
    srcs = ["sexp-parser-test.yaspl"],
    deps = [
        ":sexp-parser",
        ":yunit",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
    ],
)

yaspl_bootstrap_library(
    name = "sexp-printer",
    srcs = ["sexp-printer.yaspl"],
    deps = [
        "//libraries/data:bytes",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:numbers",
        "//libraries/formats:sexp",
    ],
)

yaspl_test(
    name = "sexp-printer-test",
    size = "small",
    srcs = ["sexp-printer-test.yaspl"],
    deps = [
        ":sexp-parser",
        ":sexp-printer",
        ":yunit",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
    ],
)

yaspl_bootstrap_library(
    name = "grammar-parser",
    srcs = ["grammar-parser.yaspl"],
    deps = [
        ":sexp-parser",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
        "//libraries/formats:sexp",
    ],
)

genrule(
    name = "gen-source-grammar",
    srcs = ["source.grammar"],
    outs = ["source-grammar.yaspl"],
    cmd = "echo '#:module (source-grammar)\n#:import {(prim)} ' \
               '(export (#:types) (#:values source-grammar) (#:patterns)) (types)' > $@;" +
          "echo -n '(define (source-grammar) : Bytes #\"' >> $@;" +
          " awk '{printf \"%s\\\\n\", $$0}' $< >> $@;" +
          " echo '\")' >> $@",
)

yaspl_bootstrap_library(
    name = "source-grammar",
    srcs = ["source-grammar.yaspl"],
    deps = [],
)

yaspl_test(
    name = "source-grammar-test",
    size = "small",
    srcs = ["source-grammar-test.yaspl"],
    deps = [
        ":grammar-parser",
        ":source-grammar",
        ":yunit",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
    ],
)

yaspl_bootstrap_library(
    name = "source-to-intermediate-language",
    srcs = ["source-to-intermediate-language.yaspl"],
    deps = [
        ":intermediate-expression",
        ":intermediate-language",
        ":module-name",
        ":module-signature",
        ":top-level-name",
        ":type-checker",
        ":types",
        ":variant-info",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:set",
        "//libraries/data:tuples",
        "//libraries/yaspl:var",
        "//libraries/yaspl/conversions:expand-datatypes",
        "//libraries/yaspl/conversions:expand-patterns",
        "//libraries/yaspl/conversions:expand-varargs",
        "//libraries/yaspl/conversions:resolve-vars",
        "//libraries/yaspl/languages:expanded-patterns-language",
        "//libraries/yaspl/languages:resolved-types-language",
    ],
)

yaspl_bootstrap_library(
    name = "intermediate-to-module-signature",
    srcs = ["intermediate-to-module-signature.yaspl"],
    deps = [
        ":intermediate-language",
        ":module-signature",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:maybe",
    ],
)

yaspl_test(
    name = "tail-call-test",
    size = "small",
    srcs = ["tail-call-test.yaspl"],
    deps = [
        ":yunit",
        "//libraries/data:list",
        "//libraries/data:maybe",
    ],
)

yaspl_library(
    name = "tcp",
    srcs = ["tcp.yaspl"],
    deps = [
        ":ip",
        ":panic",
        ":system-abi",
    ],
)

yaspl_bootstrap_library(
    name = "top-level-name",
    srcs = ["top-level-name.yaspl"],
    deps = [
        ":module-name",
        ":sexp-printer",
        "//libraries/data:bytes",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:ordering",
        "//libraries/formats:sexp",
    ],
)

yaspl_bootstrap_library(
    name = "totality-checker",
    srcs = ["totality-checker.yaspl"],
    deps = [
        ":type-checker-context",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
        "//libraries/yaspl/languages:resolved-types-language",
    ],
)

yaspl_bootstrap_library(
    name = "types",
    srcs = ["types.yaspl"],
    deps = [
        ":module-name",
        ":panic",
        ":sexp-printer",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:ordering",
        "//libraries/data:set",
        "//libraries/data:tuples",
        "//libraries/formats:sexp",
    ],
)

yaspl_test(
    name = "types-test",
    srcs = ["types-test.yaspl"],
    deps = [
        ":module-name",
        ":prim-types",
        ":sexp-printer",
        ":types",
        ":types-test-utils",
        ":yunit",
        "//libraries/data:bytes",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
        "//libraries/formats:sexp",
    ],
)

yaspl_library(
    name = "types-test-utils",
    srcs = ["types-test-utils.yaspl"],
    deps = [
        ":module-name",
        ":types",
        "//libraries/data:list",
    ],
)

yaspl_bootstrap_library(
    name = "type-checked-language",
    srcs = ["type-checked-language.yaspl"],
    deps = [
        ":module-name",
        ":module-signature",
        ":types",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/yaspl/languages:expanded-datatypes-language",
    ],
)

yaspl_bootstrap_library(
    name = "type-checker",
    srcs = ["type-checker.yaspl"],
    deps = [
        ":module-name",
        ":module-signature",
        ":prim-types",
        ":sexp-printer",
        ":totality-checker",
        ":type-checked-language",
        ":type-checker-context",
        ":type-unification",
        ":types",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:set",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
        "//libraries/yaspl/languages:expanded-datatypes-language",
    ],
)

yaspl_bootstrap_library(
    name = "type-checker-context",
    srcs = ["type-checker-context.yaspl"],
    deps = [
        ":module-name",
        ":types",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
    ],
)

yaspl_test(
    name = "type-checker-test",
    srcs = ["type-checker-test.yaspl"],
    deps = [
        ":type-checker",
        ":type-checker-context",
        ":types",
        ":types-test-utils",
        ":yunit",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
        "//libraries/yaspl/languages:resolved-types-language",
    ],
)

yaspl_bootstrap_library(
    name = "type-unification",
    srcs = ["type-unification.yaspl"],
    deps = [
        ":module-name",
        ":sexp-printer",
        ":types",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:ordering",
        "//libraries/data:set",
        "//libraries/data:tuples",
    ],
)

yaspl_test(
    name = "type-unification-test",
    srcs = ["type-unification-test.yaspl"],
    deps = [
        ":module-name",
        ":prim-types",
        ":type-unification",
        ":types",
        ":types-test-utils",
        ":yunit",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
    ],
)

yaspl_library(
    name = "udp",
    srcs = ["udp.yaspl"],
    deps = [
        ":ip",
        "//libraries/data:lifted-primitives",
        "//libraries/data:tuples",
    ],
)

yaspl_library(
    name = "unused-bindings",
    srcs = ["unused-bindings.yaspl"],
    deps = [
        ":free-variables",
        ":module-name",
        ":module-signature",
        "//libraries/algorithms:depth-first-search",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:indexed-set",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
        "//libraries/yaspl/languages:source-language",
    ],
)

yaspl_library(
    name = "unused-local-bindings",
    srcs = ["unused-local-bindings.yaspl"],
    deps = [
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
        "//libraries/yaspl/languages:source-language",
    ],
)

yaspl_bootstrap_library(
    name = "validator",
    srcs = ["validator.yaspl"],
    deps = [
        ":free-variables",
        ":module-name",
        ":module-signature",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:indexed-set",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
        "//libraries/yaspl/languages:resolved-imports-language",
    ],
)

yaspl_bootstrap_library(
    name = "variant-info",
    srcs = ["variant-info.yaspl"],
    deps = [
        ":top-level-name",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
    ],
)

yaspl_library(
    name = "yunit",
    srcs = ["yunit.yaspl"],
    deps = [
        ":io",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:tuples",
    ],
)

test_suite(
    name = "package_tests",
    testonly = 0,
)

filegroup(
    name = "package_binaries",
    srcs = [
        ":compiler",
        ":library-compiler",
        ":linker",
        ":main-stub",
    ],
)
