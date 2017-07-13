load("//libraries:yaspl.bzl", "yaspl_library", "yaspl_test", "yaspl_binary", "yaspl_bootstrap_library")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_binary(
    name = "demo",
    main_module = "prim_language_demo",
    deps = [":prim-language-demo"],
)

yaspl_binary(
    name = "prim-language-compiler",
    main_module = "prim_language_compiler_main",
    deps = [":prim-language-compiler-main"],
)

yaspl_bootstrap_library(
    name = "color-graph",
    srcs = ["color-graph.yaspl"],
    deps = [
        ":lower-x86-64",
        ":lowered-live-variables",
        ":register-language",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/data:tuples",
        "//libraries/data:undirected-graph",
    ],
)

yaspl_test(
    name = "color-graph-test",
    srcs = ["color-graph-test.yaspl"],
    main_module = "color_graph_test",
    deps = [
        ":color-graph",
        ":lower-x86-64",
        ":lowered-live-variables",
        ":prim-language",
        ":prim-to-register",
        "//libraries:yunit",
        "//libraries/data:list",
        "//libraries/data:maybe",
    ],
)

yaspl_bootstrap_library(
    name = "lower-x86-64",
    srcs = ["lower-x86-64.yaspl"],
    deps = [
        ":lowered-register-language",
        ":prim-language",
        ":register-language",
        ":syscalls",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/data:tuples",
    ],
)

yaspl_bootstrap_library(
    name = "lowered-register-language",
    srcs = ["lowered-register-language.yaspl"],
    deps = [
        ":prim-language",
        ":register-language",
        "//libraries/algorithms:find-chains",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/data:set-multi-dict",
        "//libraries/data:unique-dict",
    ],
)

yaspl_bootstrap_library(
    name = "lowered-live-variables",
    srcs = ["lowered-live-variables.yaspl"],
    deps = [
        ":lowered-register-language",
        "//libraries:graphviz",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:set",
        "//libraries/data:tuples",
        "//libraries/data:undirected-graph",
    ],
)

yaspl_bootstrap_library(
    name = "lowered-register-language-to-x86-64",
    srcs = ["lowered-register-language-to-x86-64.yaspl"],
    deps = [
        ":lower-x86-64",
        ":lowered-register-language",
        ":prim-language",
        ":register-language",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:set",
        "//libraries/data:either",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/yaspl:x86-64-assembly",
    ],
)

yaspl_bootstrap_library(
    name = "prim-language",
    srcs = ["prim-language.yaspl"],
    deps = [
        ":syscalls",
        "//libraries/data:list",
        "//libraries/data:tuples",
    ],
)

yaspl_library(
    name = "prim-language-compiler-main",
    srcs = ["prim-language-compiler-main.yaspl"],
    deps = [
        ":prim-language",
        ":prim-language-parser",
        ":prim-language-type-checker",
        ":prim-to-register",
        ":register-language-compiler",
        ":simplify-register-language",
        "//libraries:io",
        "//libraries:mach-o-writer2_lib",
        "//libraries:sexp-parser",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
        "//libraries/yaspl:x86-64-assembly",
    ],
)

yaspl_library(
    name = "prim-language-demo",
    srcs = ["prim-language-demo.yaspl"],
    deps = [
        ":color-graph",
        ":lower-x86-64",
        ":lowered-live-variables",
        ":lowered-register-language",
        ":lowered-register-language-to-x86-64",
        ":prim-language",
        ":prim-language-parser",
        ":prim-to-register",
        ":print-lowered-register-language",
        ":print-register-language",
        ":register-language",
        ":simplify-lowered-register-language",
        ":simplify-register-language",
        "//libraries:io",
        "//libraries:mach-o-writer2_lib",
        "//libraries:sexp-parser",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/yaspl:x86-64-assembly",
    ],
)

yaspl_library(
    name = "prim-language-parser",
    srcs = ["prim-language-parser.yaspl"],
    deps = [
        ":prim-language",
        ":syscalls",
        "//libraries:sexp-parser",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:tuples",
    ],
)

yaspl_library(
    name = "prim-language-type-checker",
    srcs = ["prim-language-type-checker.yaspl"],
    deps = [
        ":prim-language",
        ":syscalls",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
    ],
)

yaspl_library(
    name = "print-lowered-register-language",
    srcs = ["print-lowered-register-language.yaspl"],
    deps = [
        ":lowered-register-language",
        ":prim-language",
        ":register-language",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:numbers",
    ],
)

yaspl_library(
    name = "print-register-language",
    srcs = ["print-register-language.yaspl"],
    deps = [
        ":prim-language",
        ":register-language",
        ":syscalls",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:numbers",
    ],
)

yaspl_library(
    name = "prim-to-register",
    srcs = ["prim-to-register.yaspl"],
    deps = [
        ":prim-language",
        ":register-language",
        ":register-language-builder",
        "//libraries/data:boolean",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/data:tuples",
    ],
)

yaspl_bootstrap_library(
    name = "register-language",
    srcs = ["register-language.yaspl"],
    deps = [
        ":prim-language",
        ":syscalls",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:numbers",
    ],
)

yaspl_bootstrap_library(
    name = "register-language-builder",
    srcs = ["register-language-builder.yaspl"],
    deps = [
        ":register-language",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:tuples",
    ],
)

yaspl_bootstrap_library(
    name = "register-language-compiler",
    srcs = ["register-language-compiler.yaspl"],
    deps = [
        ":color-graph",
        ":lower-x86-64",
        ":lowered-live-variables",
        ":lowered-register-language",
        ":lowered-register-language-to-x86-64",
        ":register-language",
        ":simplify-lowered-register-language",
        ":spill-lowered-register-language",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
        "//libraries/yaspl:x86-64-assembly",
    ],
)

yaspl_bootstrap_library(
    name = "simplify-lowered-register-language",
    srcs = ["simplify-lowered-register-language.yaspl"],
    deps = [
        ":lowered-register-language",
        "//libraries/data:boolean",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/data:set-multi-dict",
        "//libraries/data:unique-dict",
    ],
)

yaspl_bootstrap_library(
    name = "simplify-register-language",
    srcs = ["simplify-register-language.yaspl"],
    deps = [
        ":prim-language",
        ":register-language",
        "//libraries/data:dict",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/data:tuples",
    ],
)

yaspl_bootstrap_library(
    name = "spill-lowered-register-language",
    srcs = ["spill-lowered-register-language.yaspl"],
    deps = [
        ":lower-x86-64",
        ":lowered-live-variables",
        ":lowered-register-language",
        ":register-language",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/data:tuples",
    ],
)

yaspl_bootstrap_library(
    name = "syscalls",
    srcs = ["syscalls.yaspl"],
    deps = [
    ],
)

################################################################################
load(":prim-language.bzl", "prim_binary", "binary_test")

prim_binary(
    name = "call1",
    srcs = ["call1.prim"],
)

binary_test(
    name = "call1_test",
    binary = ":call1",
    binary_args = ["arg1"],
    exit_code = 42,
)

prim_binary(
    name = "cast1",
    srcs = ["cast1.prim"],
)

binary_test(
    name = "cast1_test",
    binary = ":cast1",
    exit_code = 30,
)

prim_binary(
    name = "exit",
    srcs = ["exit.prim"],
)

binary_test(
    name = "exit_test",
    binary = ":exit",
    exit_code = 42,
)

prim_binary(
    name = "factorial",
    srcs = ["factorial.prim"],
)

binary_test(
    name = "factorial_test",
    binary = ":factorial",
    binary_args = [
        "2",
        "3",
        "4",
    ],
    exit_code = 5,
)

prim_binary(
    name = "hello-world",
    srcs = ["hello-world.prim"],
)

binary_test(
    name = "hello-world_test",
    binary = ":hello-world",
    exit_code = 6,
    output = "Hello",
)

prim_binary(
    name = "pointers1",
    srcs = ["pointers1.prim"],
)

binary_test(
    name = "pointers1_test",
    binary = ":pointers1",
    exit_code = 4,
)

prim_binary(
    name = "pointers2",
    srcs = ["pointers2.prim"],
)

binary_test(
    name = "pointers2_test",
    binary = ":pointers2",
    exit_code = 3,
)

prim_binary(
    name = "spill",
    srcs = ["spill.prim"],
)

binary_test(
    name = "spill_test",
    binary = ":spill",
    exit_code = 6,
)
