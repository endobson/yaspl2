load("//:racket.bzl", "racket_test", "racket_binary", "racket_library")
package(
  default_visibility = ["//visibility:public"]
)

racket_binary(
  name = "compiler-compiler_bin",
  main_module = 
    ":compiler-compiler.rkt",
  data = [
    "//libraries:compiler_library_files"
  ],
  deps = [":compiler-compiler"],
)


racket_library(
  name = "compiler-compiler",
  srcs = [
    "compiler-compiler.rkt"
  ],
  deps = [
    ":compiler",
    ":libraries"
  ]
)


racket_library(
  name = "parser-structs",
  srcs = [
    "parser-structs.rkt"
  ]
)

racket_library(
  name = "parser",
  srcs = [
    "parser.rkt"
  ],
  deps = [
    ":parser-structs"
  ]
)


racket_library(
  name = "machine-structs",
  srcs = [
    "machine-structs.rkt"
  ]
)

racket_library(
  name = "signature-structs",
  srcs = [
    "signature-structs.rkt"
  ]
)

racket_library(
  name = "primitives",
  srcs = [
    "primitives.rkt"
  ],
  deps = [
    ":machine-structs",
    ":type-structs",
    ":signature-structs",
  ]
)

racket_library(
  name = "simple-match",
  srcs = [
    "simple-match.rkt"
  ],
  deps = [
    ":machine-structs",
    ":parser-structs",
  ]
)

racket_library(
  name = "validator",
  srcs = [
    "validator.rkt"
  ],
  deps = [
    ":parser-structs",
    ":type-structs",
    ":signature-structs",
    ":utils",
  ]
)

racket_library(
  name = "libraries",
  srcs = [
    "libraries.rkt"
  ],
  deps = [
    ":parser",
    ":parser-structs",
    ":primitives",
    ":topo-sort",
    ":validator",
  ]
)


racket_library(
  name = "compiler",
  srcs = [
    "compiler.rkt"
  ],
  deps = [
    ":machine-structs",
    ":parser-structs",
    ":primitives",
    ":utils",
    ":topo-sort",
    ":simple-match",
  ]
)





racket_library(
  name = "topo-sort",
  srcs = [
    "topo-sort.rkt"
  ],
  deps = [
    ":parser-structs",
  ]
)


racket_library(
  name = "utils",
  srcs = [
    "utils.rkt"
  ]
)



racket_library(
  name = "type-structs",
  srcs = [
    "type-structs.rkt"
  ]
)

genrule(
  name = "gen_bootstrap_compiler",
  tools = [":compiler-compiler_bin"],
  outs = ["bootstrap_compiler.s"],
  cmd = "$(location :compiler-compiler_bin) $@"
)

genrule(
  name = "assemble_bootstrap_compiler",
  srcs = ["bootstrap_compiler.s"],
  outs = ["bootstrap_compiler.o"],
  cmd = "as $(location bootstrap_compiler.s) -o $(location bootstrap_compiler.o)"
)


genrule(
  name = "link_bootstrap_compiler",
  srcs = ["bootstrap_compiler.o"],
  outs = ["bootstrap_compiler"],
  executable = 1,
  output_to_bindir = 1,
  cmd = "ld -arch x86_64 " +
    "-macosx_version_min 10.11 " +
    "-static " +
    "-sectcreate __DATA __data /dev/null " +
    "$(location bootstrap_compiler.o) -o $(location bootstrap_compiler)"
)

sh_test(
  name = "compile-compiler-test",
  srcs = ["compile-compiler-test.sh"],
  data = [
    ":bootstrap_compiler.s",
    "//libraries:compiler.s"
  ],
)
