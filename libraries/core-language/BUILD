load(
    "//libraries:yaspl.bzl",
    "yaspl_binary",
    "yaspl_library",
    "yaspl_test",
)

package(
    default_visibility = ["//visibility:public"],
)

test_suite(
    name = "package_tests",
    testonly = 0,
)

genrule(
    name = "compile-example",
    srcs = ["examples/fill-stack.core"],
    outs = [
        "fill-stack.o",
        "fill-stack.sig",
    ],
    cmd = "$(execpath :compiler) osx $(execpath fill-stack.o) " +
          "$(execpath fill-stack.sig) $(execpath examples/fill-stack.core)",
    tools = [":compiler"],
)

yaspl_binary(
    name = "compiler",
    srcs = ["compiler.yaspl"],
    deps = [
        ":lexer",
        "//libraries:compiler_lib",
        "//libraries:io",
        "//libraries:machine-code-writer",
        "//libraries:system-abi",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
        "//libraries/yaspl:parser",
        "//libraries/yaspl:top-level-objects-to-x86-64",
        "//libraries/yaspl/frontend:errors",
    ],
)

yaspl_library(
    name = "lexer",
    srcs = ["lexer.yaspl"],
    deps = [
        "//libraries:byte-escapes",
        "//libraries:lexer",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:numbers",
        "//libraries/data:source-location",
    ],
)

yaspl_test(
    name = "lexer-test",
    size = "small",
    srcs = ["lexer-test.yaspl"],
    deps = [
        ":lexer",
        "//libraries:yunit",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:source-location",
    ],
)

filegroup(
    name = "package_binaries",
    srcs = [
        ":compiler",
    ],
)
