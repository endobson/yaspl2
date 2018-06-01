load("//libraries:yaspl.bzl", "yaspl_library", "yaspl_binary")
load("@minimal_racket//:racket.bzl", "racket_binary", "racket_library")

yaspl_binary(
    name = "rewrite-imports",
    main_module = "tools_rewrite_imports",
    deps = [":rewrite-imports-main"],
)

yaspl_binary(
    name = "aspect-linter",
    main_module = "tools_aspect_linter_main",
    visibility = ["//visibility:public"],
    deps = [":aspect-linter-main"],
)

yaspl_binary(
    name = "aspect-analysis",
    main_module = "tools_aspect_analysis_main",
    visibility = ["//visibility:public"],
    deps = [":aspect-analysis-main"],
)

yaspl_binary(
    name = "ir-printer",
    main_module = "tools_ir_printer_main",
    visibility = ["//visibility:public"],
    deps = [":ir-printer-main"],
)

yaspl_binary(
    name = "intermediate-html-printer",
    main_module = "tools_intermediate_html_printer_main",
    visibility = ["//visibility:public"],
    deps = [":intermediate-html-printer-main"],
)

racket_binary(
    name = "fs-event",
    main_module = ":fs-event-main.rkt",
    deps = [":fs-event-main"],
)

yaspl_library(
    name = "aspect-linter-main",
    srcs = ["aspect-linter-main.yaspl"],
    deps = [
        ":linter_lib",
        "//libraries:io",
        "//libraries/data:either",
        "//libraries/data:list",
    ],
)

yaspl_library(
    name = "aspect-analysis-main",
    srcs = ["aspect-analysis-main.yaspl"],
    deps = [
        ":linter_lib",
        "//libraries:io",
        "//libraries/data:either",
        "//libraries/data:list",
    ],
)

yaspl_library(
    name = "compile-module-args",
    srcs = ["compile-module-args.yaspl"],
    deps = [
        "//libraries:compiler_lib",
        "//libraries:io",
        "//libraries:module-signature",
        "//libraries:module-signature-serialization",
        "//libraries:source-language",
        "//libraries/data:either",
        "//libraries/data:list",
    ],
)

racket_library(
    name = "fs-event-main",
    srcs = ["fs-event-main.rkt"],
)

yaspl_library(
    name = "intermediate-html-printer-main",
    srcs = ["intermediate-html-printer-main.yaspl"],
    deps = [
        ":compile-module-args",
        "//libraries:io",
        "//libraries:module-name",
        "//libraries:module-signature",
        "//libraries:prim-implementation",
        "//libraries:resolved-imports-language",
        "//libraries:source-to-intermediate-language",
        "//libraries:validator",
        "//libraries/data:either",
        "//libraries/data:indexed-set",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/formats:xml",
        "//libraries/yaspl/debug:intermediate-to-html",
    ],
)

yaspl_library(
    name = "ir-printer-main",
    srcs = ["ir-printer-main.yaspl"],
    deps = [
        ":compile-module-args",
        "//libraries:compiler_lib",
        "//libraries:io",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:tuples",
        "//libraries/prim-language:color-graph",
        "//libraries/prim-language:lower-x86-64",
        "//libraries/prim-language:lowered-live-variables",
        "//libraries/prim-language:print-lowered-register-language",
        "//libraries/prim-language:print-register-language",
        "//libraries/prim-language:register-language",
        "//libraries/prim-language:register-language-compiler",
        "//libraries/yaspl:top-level-objects",
        "//libraries/yaspl:top-level-objects-to-x86-64",
        "//libraries/yaspl:var",
        "//libraries/yaspl:x86-64-assembly",
        "//libraries/yaspl:x86-64-assembly-printer",
    ],
)

yaspl_library(
    name = "linter_lib",
    srcs = ["linter.yaspl"],
    deps = [
        "//libraries:io",
        "//libraries:module-header-lint",
        "//libraries:module-name",
        "//libraries:module-signature",
        "//libraries:module-signature-serialization",
        "//libraries:sexp-parser",
        "//libraries:source-language",
        "//libraries:unused-bindings",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:indexed-set",
        "//libraries/data:list",
        "//libraries/data:set",
    ],
)

yaspl_library(
    name = "rewrite-imports-main",
    srcs = ["rewrite-imports.yaspl"],
    deps = [
        "//libraries:compiler_lib",
        "//libraries:io",
        "//libraries:module-name",
        "//libraries:sexp-parser",
        "//libraries:source-language",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:tuples",
    ],
)
