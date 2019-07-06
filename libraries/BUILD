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
        ":machine-code",
        ":machine-code-writer",
        ":module-name",
        ":top-level-name",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
        "//libraries/prim-language:prim-language-compiler_lib",
        "//libraries/yaspl:top-level-objects-to-x86-64",
        "//libraries/yaspl:x86-64-assembly",
        "//libraries/yaspl:x86-64-runtime",
    ],
)

yaspl_bootstrap_binary(
    name = "library-compiler",
    srcs = ["library-compiler-main.yaspl"],
    deps = [
        ":compiler_lib",
        ":io",
        ":machine-code",
        ":machine-code-writer",
        ":module-signature-serialization",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
        "//libraries/yaspl:top-level-objects-to-x86-64",
    ],
)

yaspl_bootstrap_binary(
    name = "linker",
    srcs = ["linker-main.yaspl"],
    deps = [
        ":io",
        ":mach-o",
        ":mach-o-writer",
        ":structured-elf",
        ":structured-mach-o",
        ":structured-native-code",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/formats:elf",
        "//libraries/formats:elf-writer",
    ],
)

yaspl_bootstrap_binary(
    name = "main-stub",
    srcs = ["main-stub-main.yaspl"],
    deps = [
        ":io",
        ":machine-code",
        ":machine-code-writer",
        ":module-name",
        ":prim-implementation",
        ":sexp-parser",
        ":top-level-name",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
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
        ":panic",
        "//libraries/data:bytes",
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
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
    ],
)

yaspl_library(
    name = "stack-trace",
    srcs = ["stack-trace.yaspl"],
    deps = [
        ":in-memory-mach-o",
        ":mach-o",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:ordering",
        "//libraries/data:red-black-tree",
        "//libraries/data:tuples",
    ],
)

yaspl_test(
    name = "fill-stack-test",
    size = "small",
    srcs = ["fill-stack-test.yaspl"],
    tags = [
        "manual",
    ],
    deps = [
        ":io",
        ":stack-trace",
        "//libraries/data:bytes",
        "//libraries/data:list",
        "//libraries/data:tuples",
    ],
)

yaspl_library(
    name = "module-header-lint",
    srcs = ["module-header-lint.yaspl"],
    deps = [
        ":module-name",
        ":source-language",
        "//libraries/data:bytes",
        "//libraries/data:list",
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
        ":source-language",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/data:source-location",
    ],
)

yaspl_library(
    name = "http-server",
    srcs = ["http-server.yaspl"],
    deps = [
        ":buffered-port",
        ":io",
        ":tcp",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:regexp",
        "//libraries/protocols/http:http-response",
    ],
)

yaspl_bootstrap_library(
    name = "intermediate-expression",
    srcs = ["intermediate-expression.yaspl"],
    deps = [
        ":top-level-name",
        ":types",
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
        ":byte-escapes",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:range-map",
        "//libraries/data:source-location",
    ],
)

yaspl_test(
    name = "lexer-test",
    size = "small",
    srcs = ["lexer-test.yaspl"],
    deps = [
        ":lexer",
        ":yunit",
        "//libraries/data:bytes",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
    ],
)

yaspl_bootstrap_library(
    name = "machine-code",
    srcs = ["machine-code.yaspl"],
)

yaspl_bootstrap_library(
    name = "machine-code-writer",
    srcs = ["machine-code-writer.yaspl"],
    deps = [
        ":assembler",
        ":io",
        ":mach-o-writer",
        ":machine-code",
        "//libraries/data:maybe",
        "//libraries/formats:elf-writer",
        "//libraries/yaspl:x86-64-assembly",
        "//libraries/yaspl:x86-64-assembly-printer",
    ],
)

yaspl_bootstrap_library(
    name = "compiler_lib",
    srcs = ["compiler.yaspl"],
    deps = [
        ":intermediate-language",
        ":intermediate-to-module-signature",
        ":io",
        ":module-name",
        ":module-signature",
        ":module-signature-serialization",
        ":prim-implementation",
        ":resolved-imports-language",
        ":resolved-types-language",
        ":sexp-parser",
        ":source-language",
        ":source-to-intermediate-language",
        ":validator",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:indexed-set",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
        "//libraries/yaspl:intermediate-to-register",
        "//libraries/yaspl:optimize-intermediate",
        "//libraries/yaspl:top-level-object-optimizer",
        "//libraries/yaspl:top-level-objects",
    ],
)

yaspl_bootstrap_library(
    name = "expanded-datatypes-language",
    srcs = ["expanded-datatypes-language.yaspl"],
    deps = [
        ":module-name",
        ":type-checked-language",
        ":types",
        "//libraries/data:list",
        "//libraries/data:source-location",
    ],
)

yaspl_bootstrap_library(
    name = "expanded-patterns-language",
    srcs = ["expanded-patterns-language.yaspl"],
    deps = [
        ":module-name",
        ":module-signature",
        ":panic",
        ":resolved-vars-language",
        ":top-level-name",
        ":types",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/data:tuples",
        "//libraries/yaspl:var",
    ],
)

yaspl_bootstrap_library(
    name = "linker_lib",
    srcs = ["linker.yaspl"],
    deps = [
        ":mach-o",
        ":structured-mach-o",
        ":structured-native-code",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:indexed-set",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:ordering",
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

yaspl_bootstrap_library(
    name = "mach-o",
    srcs = ["mach-o.yaspl"],
    deps = [
        ":io",
        ":panic",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:numbers",
        "//libraries/data:tuples",
    ],
)

yaspl_library(
    name = "in-memory-mach-o",
    srcs = ["in-memory-mach-o.yaspl"],
    deps = [
        ":mach-o",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:tuples",
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
        ":mach-o",
        ":structured-native-code",
        "//libraries/data:aligned-bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
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
    name = "mach-o-writer",
    srcs = ["mach-o-writer.yaspl"],
    deps = [
        ":io",
        ":linker_lib",
        ":mach-o",
        ":panic",
        ":structured-mach-o",
        ":structured-native-code",
        "//libraries/data:aligned-bytes",
        "//libraries/data:bytes",
        "//libraries/data:indexed-set",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:set",
    ],
)

yaspl_bootstrap_library(
    name = "module-name",
    srcs = ["module-name.yaspl"],
    deps = [
        ":sexp-parser",
        ":sexp-printer",
        "//libraries/data:bytes",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:ordering",
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
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
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
        ":yunit",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
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
    name = "prim-types",
    srcs = ["prim-types.yaspl"],
    deps = [
        ":module-name",
        ":types",
        "//libraries/data:list",
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
    name = "resolved-imports-language",
    srcs = ["resolved-imports-language.yaspl"],
    deps = [
        ":module-name",
        ":module-signature",
        ":source-language",
        ":top-level-name",
        ":types",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:indexed-set",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/data:tuples",
    ],
)

yaspl_test(
    name = "resolved-imports-language-test",
    size = "small",
    srcs = ["resolved-imports-language-test.yaspl"],
    deps = [
        ":module-name",
        ":module-signature",
        ":resolved-imports-language",
        ":source-language",
        ":top-level-name",
        ":yunit",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:indexed-set",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
    ],
)

yaspl_bootstrap_library(
    name = "resolved-types-language",
    srcs = ["resolved-types-language.yaspl"],
    deps = [
        ":module-name",
        ":module-signature",
        ":resolved-imports-language",
        ":sexp-printer",
        ":top-level-name",
        ":types",
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
    name = "resolved-vars-language",
    srcs = ["resolved-vars-language.yaspl"],
    deps = [
        ":expanded-datatypes-language",
        ":module-name",
        ":module-signature",
        ":top-level-name",
        ":types",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
        "//libraries/yaspl:var",
    ],
)

yaspl_bootstrap_library(
    name = "sexp-parser",
    srcs = ["sexp-parser.yaspl"],
    deps = [
        ":lexer",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:source-location",
    ],
)

yaspl_test(
    name = "sexp-parser-test",
    size = "small",
    srcs = ["sexp-parser-test.yaspl"],
    deps = [
        ":sexp-parser",
        ":yunit",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:source-location",
    ],
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
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
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
    name = "source-language",
    srcs = ["source-language.yaspl"],
    deps = [
        ":module-name",
        ":sexp-parser",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
    ],
)

yaspl_test(
    name = "source-language-test",
    size = "small",
    srcs = ["source-language-test.yaspl"],
    deps = [
        ":sexp-parser",
        ":source-language",
        ":yunit",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
    ],
)

yaspl_bootstrap_library(
    name = "source-to-intermediate-language",
    srcs = ["source-to-intermediate-language.yaspl"],
    deps = [
        ":expanded-datatypes-language",
        ":expanded-patterns-language",
        ":intermediate-expression",
        ":intermediate-language",
        ":module-name",
        ":module-signature",
        ":resolved-types-language",
        ":resolved-vars-language",
        ":top-level-name",
        ":type-checker",
        ":types",
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
    ],
)

yaspl_bootstrap_library(
    name = "intermediate-to-module-signature",
    srcs = ["intermediate-to-module-signature.yaspl"],
    deps = [
        ":intermediate-language",
        ":module-signature",
        ":types",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:list",
    ],
)

yaspl_test(
    name = "tail-call-test",
    size = "small",
    srcs = ["tail-call-test.yaspl"],
    deps = [
        ":yunit",
        "//libraries/data:list",
    ],
)

yaspl_library(
    name = "tcp",
    srcs = ["tcp.yaspl"],
    deps = [
        ":ip",
        ":machine-code",
    ],
)

yaspl_bootstrap_library(
    name = "top-level-name",
    srcs = ["top-level-name.yaspl"],
    deps = [
        ":module-name",
        ":sexp-parser",
        ":sexp-printer",
        "//libraries/data:bytes",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:ordering",
    ],
)

yaspl_bootstrap_library(
    name = "totality-checker",
    srcs = ["totality-checker.yaspl"],
    deps = [
        ":module-signature",
        ":resolved-types-language",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
    ],
)

yaspl_bootstrap_library(
    name = "types",
    srcs = ["types.yaspl"],
    deps = [
        ":module-name",
        ":sexp-parser",
        ":sexp-printer",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:ordering",
        "//libraries/data:tuples",
    ],
)

yaspl_test(
    name = "types-test",
    srcs = ["types-test.yaspl"],
    deps = [
        ":module-name",
        ":sexp-parser",
        ":sexp-printer",
        ":types",
        ":types-test-utils",
        ":yunit",
        "//libraries/data:bytes",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
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
        ":resolved-types-language",
        ":types",
        "//libraries/data:list",
    ],
)

yaspl_bootstrap_library(
    name = "type-checker",
    srcs = ["type-checker.yaspl"],
    deps = [
        ":module-name",
        ":module-signature",
        ":prim-types",
        ":resolved-types-language",
        ":sexp-printer",
        ":totality-checker",
        ":type-checked-language",
        ":type-checker-context",
        ":type-unification",
        ":types",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:set",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
    ],
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
    ],
)

yaspl_test(
    name = "type-checker-test",
    srcs = ["type-checker-test.yaspl"],
    deps = [
        ":resolved-types-language",
        ":type-checker",
        ":type-checker-context",
        ":types",
        ":types-test-utils",
        ":yunit",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
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
        ":source-language",
        "//libraries/algorithms:depth-first-search",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:indexed-set",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/data:tuples",
    ],
)

yaspl_library(
    name = "unused-local-bindings",
    srcs = ["unused-local-bindings.yaspl"],
    deps = [
        ":source-language",
        "//libraries/data:bytes",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/data:tuples",
    ],
)

yaspl_bootstrap_library(
    name = "validator",
    srcs = ["validator.yaspl"],
    deps = [
        ":free-variables",
        ":module-name",
        ":module-signature",
        ":resolved-imports-language",
        ":source-language",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:indexed-set",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:multi-set",
        "//libraries/data:numbers",
        "//libraries/data:set",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
    ],
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
