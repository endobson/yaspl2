load("@minimal_racket//:racket.bzl", "racket_binary", "racket_library")
load(":bootstrap.bzl", "bootstrap_binary")

package(
    default_visibility = ["//visibility:public"],
)

racket_library(
    name = "parser-structs",
    srcs = ["parser-structs.rkt"],
)

racket_library(
    name = "parser",
    srcs = ["parser.rkt"],
    deps = [
        ":parser-structs",
        ":utils",
    ],
)

racket_library(
    name = "machine-structs",
    srcs = ["machine-structs.rkt"],
)

racket_library(
    name = "signature-structs",
    srcs = ["signature-structs.rkt"],
)

racket_library(
    name = "primitives",
    srcs = ["primitives.rkt"],
    deps = [
        ":machine-structs",
        ":parser-structs",
        ":signature-structs",
        ":type-structs",
    ],
)

racket_library(
    name = "validator",
    srcs = ["validator.rkt"],
    deps = [
        ":parser-structs",
        ":signature-structs",
        ":type-structs",
        ":utils",
    ],
)

racket_library(
    name = "libraries",
    srcs = ["libraries.rkt"],
    deps = [
        ":parser",
        ":parser-structs",
        ":primitives",
        ":topo-sort",
        ":validator",
    ],
)

racket_library(
    name = "compiler",
    srcs = ["compiler.rkt"],
    deps = [
        ":machine-structs",
        ":parser-structs",
        ":primitives",
        ":topo-sort",
        ":utils",
        "//bootstrap/racketize:module",
        "//bootstrap/racketize:pattern",
    ],
)

racket_library(
    name = "bootstrap-compiler",
    srcs = ["bootstrap-compiler.rkt"],
    data = [
        "//libraries:compiler_library_files",
        "//libraries/yaspl/runtime:runtime-sources",
        "//libraries/yaspl/runtime:runtime-sources.list",
    ],
    deps = [
        ":compiler",
        ":libraries",
    ],
)

racket_library(
    name = "bootstrapper-main",
    srcs = ["bootstrapper-main.rkt"],
    deps = [
        ":bootstrap-compiler",
    ],
)

racket_binary(
    name = "bootstrapper",
    main_module = "bootstrapper-main.rkt",
    deps = [
        ":bootstrapper-main",
    ],
)

racket_library(
    name = "topo-sort",
    srcs = ["topo-sort.rkt"],
    deps = [
        ":parser-structs",
    ],
)

racket_library(
    name = "utils",
    srcs = ["utils.rkt"],
)

racket_library(
    name = "type-structs",
    srcs = ["type-structs.rkt"],
)

bootstrap_binary(
    name = "bootstrap_library_compiler",
    source_files = "//libraries:library-compiler_library_files",
)

bootstrap_binary(
    name = "bootstrap_main_stub",
    source_files = "//libraries:main-stub_library_files",
)

bootstrap_binary(
    name = "bootstrap_linker",
    source_files = "//libraries:linker_library_files",
)

bootstrap_binary(
    name = "bootstrap_prim_language_library_compiler",
    source_files = "//libraries/prim-language:prim-language-library-compiler_library_files",
)
