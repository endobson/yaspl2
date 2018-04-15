load("@minimal_racket//:racket.bzl", "racket_binary", "racket_library")

package(
    default_visibility = ["//visibility:public"],
)

racket_binary(
    name = "library-compiler-compiler_bin",
    data = [
        "//libraries:compiler_library_files",
        "//libraries:library_compiler_library_files",
    ],
    main_module = ":library-compiler-compiler.rkt",
    deps = [":library-compiler-compiler"],
)

racket_binary(
    name = "main-stub-compiler_bin",
    data = [
        "//libraries:compiler_library_files",
        "//libraries:main_stub_library_files",
    ],
    main_module = ":main-stub-compiler.rkt",
    deps = [":main-stub-compiler"],
)

racket_binary(
    name = "linker-compiler_bin",
    data = [
        "//libraries:compiler_library_files",
        "//libraries:linker_library_files",
    ],
    main_module = ":linker-compiler.rkt",
    deps = [":linker-compiler"],
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

genrule(
    name = "gen_bootstrap_library_compiler",
    outs = ["bootstrap_library_compiler.s"],
    cmd = "$(location :library-compiler-compiler_bin) $@",
    tools = [":library-compiler-compiler_bin"],
)

genrule(
    name = "assemble_bootstrap_library_compiler",
    srcs = ["bootstrap_library_compiler.s"],
    outs = ["bootstrap_library_compiler.o"],
    cmd = "as $(location bootstrap_library_compiler.s) -o $(location bootstrap_library_compiler.o)",
)

genrule(
    name = "link_bootstrap_library_compiler",
    srcs = ["bootstrap_library_compiler.o"],
    outs = ["bootstrap_library_compiler"],
    cmd = "ld -arch x86_64 " +
          "-macosx_version_min 10.11 " +
          "-static " +
          "-no_uuid " +
          "-sectcreate __DATA __data /dev/null " +
          "$(location bootstrap_library_compiler.o) -o $(location bootstrap_library_compiler)",
    executable = 1,
    output_to_bindir = 1,
)

genrule(
    name = "gen_bootstrap_main_stub",
    outs = ["bootstrap_main_stub.s"],
    cmd = "$(location :main-stub-compiler_bin) $@",
    tools = [":main-stub-compiler_bin"],
)

genrule(
    name = "assemble_bootstrap_main_stub",
    srcs = ["bootstrap_main_stub.s"],
    outs = ["bootstrap_main_stub.o"],
    cmd = "as $(location bootstrap_main_stub.s) -o $(location bootstrap_main_stub.o)",
)

genrule(
    name = "link_bootstrap_main_stub",
    srcs = ["bootstrap_main_stub.o"],
    outs = ["bootstrap_main_stub"],
    cmd = "ld -arch x86_64 " +
          "-macosx_version_min 10.11 " +
          "-static " +
          "-no_uuid " +
          "-sectcreate __DATA __data /dev/null " +
          "$(location bootstrap_main_stub.o) -o $(location bootstrap_main_stub)",
    executable = 1,
    output_to_bindir = 1,
)

genrule(
    name = "gen_bootstrap_linker",
    outs = ["bootstrap_linker.s"],
    cmd = "$(location :linker-compiler_bin) $@",
    tools = [":linker-compiler_bin"],
)

genrule(
    name = "assemble_bootstrap_linker",
    srcs = ["bootstrap_linker.s"],
    outs = ["bootstrap_linker.o"],
    cmd = "as $(location bootstrap_linker.s) -o $(location bootstrap_linker.o)",
)

genrule(
    name = "link_bootstrap_linker",
    srcs = ["bootstrap_linker.o"],
    outs = ["bootstrap_linker"],
    cmd = "ld -arch x86_64 " +
          "-macosx_version_min 10.11 " +
          "-static " +
          "-no_uuid " +
          "-sectcreate __DATA __data /dev/null " +
          "$(location bootstrap_linker.o) -o $(location bootstrap_linker)",
    executable = 1,
    output_to_bindir = 1,
)

genrule(
    name = "gen_compiler_binaries",
    outs = ["compiler_binaries"],
    cmd = "touch $@",
    tools = [
        ":bootstrap_library_compiler",
        ":bootstrap_linker",
        ":bootstrap_main_stub",
    ],
)

filegroup(
    name = "library_compiler",
    srcs = select({
        ":yaspl_bootstrap": [":bootstrap_library_compiler"],
        "//conditions:default": [":saved_library_compiler"],
    }),
)

filegroup(
    name = "main_stub",
    srcs = select({
        ":yaspl_bootstrap": [":bootstrap_main_stub"],
        "//conditions:default": [":saved_main_stub"],
    }),
)

filegroup(
    name = "linker",
    srcs = select({
        ":yaspl_bootstrap": [":bootstrap_linker"],
        "//conditions:default": [":saved_linker"],
    }),
)

config_setting(
    name = "yaspl_bootstrap",
    values = {"define": "yaspl_bootstrap=true"},
)
