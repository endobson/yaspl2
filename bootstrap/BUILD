load("//:racket.bzl", "racket_test", "racket_binary", "racket_library")
package(
  default_visibility = ["//visibility:public"]
)

racket_test(
  name = "tests_rkt",
  srcs = [
    "tests.rkt",
  ],
  data = [
    "//libraries:compiler_library_files",
    "//tests:files"
  ],
  deps = glob(["*.rkt"]),
)

racket_binary(
  name = "compiler-compiler",
  srcs = [
    "compiler-compiler.rkt"
  ],
  data = glob(["tests/*"]) + [
    "//libraries:compiler_library_files"
  ],
  deps = glob(["*.rkt"]),
)


racket_library(
  name = "parser-structs",
  srcs = [
    "parser-structs.rkt"
  ]
)


genrule(
  name = "gen_compiler",
  tools = [":compiler-compiler"],
  outs = ["compiler.s"],
  cmd = "$(location :compiler-compiler) $@"
)

genrule(
  name = "assemble_compiler",
  srcs = ["compiler.s"],
  outs = ["compiler.o"],
  cmd = "as $(location compiler.s) -o $(location compiler.o)"
)


genrule(
  name = "link_compiler",
  srcs = ["compiler.o"],
  outs = ["compiler"],
  executable = 1,
  output_to_bindir = 1,
  cmd = "ld -arch x86_64 " +
    "-macosx_version_min 10.11 " +
    "-static " +
    "$(location compiler.o) -o $(location compiler)"
)

sh_test(
  name = "compile-compiler-test",
  srcs = ["compile-compiler-test.sh"],
  data = [
    ":compiler.s",
    "//libraries:compiler.s"
  ],
)
