load(
    "//libraries:yaspl.bzl",
    "yaspl_binary",
    "yaspl_library",
    "yaspl_test",
)

package(
    default_visibility = ["//visibility:public"],
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
        ":core-parser",
        ":lexer",
        ":lr-parser",
        "//libraries:io",
        "//libraries:machine-code-writer",
        "//libraries:system-abi",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
    ],
)

yaspl_library(
    name = "core-parser",
    srcs = ["core-parser.yaspl"],
    deps = [
        ":lexer",
        ":lr-parser",
        ":source-language",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:numbers",
        "//libraries/data:ordering",
        "//libraries/data:tuples",
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
        "//libraries/data:ordering",
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

yaspl_library(
    name = "lr-parser",
    srcs = ["lr-parser.yaspl"],
    deps = [
        ":lexer",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:ordering",
        "//libraries/data:set",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
        "//libraries/data:unique-dict",
    ],
)

yaspl_test(
    name = "lr-parser-test",
    size = "small",
    srcs = ["lr-parser-test.yaspl"],
    deps = [
        ":lexer",
        ":lr-parser",
        "//libraries:yunit",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:ordering",
        "//libraries/data:source-location",
        "//libraries/data:tuples",
    ],
)

yaspl_library(
    name = "source-language",
    srcs = ["source-language.yaspl"],
    deps = ["//libraries/data:list"],
)

filegroup(
    name = "package_binaries",
    srcs = [
        ":compiler",
    ],
)

test_suite(
    name = "package_tests",
    testonly = 0,
)
