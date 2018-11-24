load("@minimal_racket//:racket.bzl", "racket_binary", "racket_library")

load(":bootstrap.bzl", "bootstrap_binary")

package(
    default_visibility = ["//visibility:public"],
)

racket_binary(
    name = "library-compiler-compiler_bin",
    data = [
        "//libraries:library-compiler_library_files",
    ],
    main_module = ":library-compiler-compiler.rkt",
    deps = [":library-compiler-compiler"],
)

racket_binary(
    name = "main-stub-compiler_bin",
    data = [
        "//libraries:main-stub_library_files",
    ],
    main_module = ":main-stub-compiler.rkt",
    deps = [":main-stub-compiler"],
)

racket_binary(
    name = "linker-compiler_bin",
    data = [
        "//libraries:linker_library_files",
    ],
    main_module = ":linker-compiler.rkt",
    deps = [":linker-compiler"],
)

racket_binary(
    name = "prim-language-library-compiler-compiler_bin",
    data = [
        "//libraries/prim-language:prim-language-library-compiler_library_files",
    ],
    main_module = ":prim-language-library-compiler-compiler.rkt",
    deps = [":prim-language-library-compiler-compiler"],
)

racket_library(
    name = "library-compiler-compiler",
    srcs = ["library-compiler-compiler.rkt"],
    deps = [
        ":bootstrap-compiler",
    ],
)

racket_library(
    name = "main-stub-compiler",
    srcs = ["main-stub-compiler.rkt"],
    deps = [
        ":bootstrap-compiler",
    ],
)

racket_library(
    name = "linker-compiler",
    srcs = ["linker-compiler.rkt"],
    deps = [
        ":bootstrap-compiler",
    ],
)

racket_library(
    name = "prim-language-library-compiler-compiler",
    srcs = ["prim-language-library-compiler-compiler.rkt"],
    deps = [
        ":bootstrap-compiler",
    ],
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
    name = "simple-match",
    srcs = ["simple-match.rkt"],
    deps = [
        ":machine-structs",
        ":parser-structs",
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
        ":simple-match",
        ":topo-sort",
        ":utils",
    ],
)

racket_library(
    name = "bootstrap-compiler",
    srcs = ["bootstrap-compiler.rkt"],
    data = [
        "//libraries:compiler_library_files",
        "//libraries/yaspl/runtime:runtime-sources",
    ],
    deps = [
        ":compiler",
        ":libraries",
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
    generator = ":library-compiler-compiler_bin"
)

bootstrap_binary(
    name = "bootstrap_main_stub",
    generator = ":main-stub-compiler_bin"
)

bootstrap_binary(
    name = "bootstrap_linker",
    generator = ":linker-compiler_bin"
)

bootstrap_binary(
    name = "bootstrap_prim_language_library_compiler",
    generator = ":prim-language-library-compiler-compiler_bin"
)
