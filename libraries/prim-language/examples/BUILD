load("//libraries/prim-language:prim-language.bzl", "binary_test", "prim_binary", "prim_library")

package(
    default_visibility = ["//visibility:public"],
)

prim_library(
    name = "call1_lib",
    srcs = ["call1.prim"],
)

prim_binary(
    name = "call1",
    srcs = [
        "call1.prim",
    ],
)

binary_test(
    name = "call1_test",
    binary = ":call1",
    binary_args = ["arg1"],
    exit_code = 42,
)

prim_binary(
    name = "call2",
    srcs = ["call2.prim"],
)

binary_test(
    name = "call2_test",
    binary = ":call2",
    binary_args = ["arg1"],
    exit_code = 14,
)

prim_binary(
    name = "call3",
    srcs = ["call3.prim"],
)

binary_test(
    name = "call3_test",
    binary = ":call3",
    binary_args = ["arg1"],
    exit_code = 3,
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
    name = "pointers3",
    srcs = ["pointers3.prim"],
)

binary_test(
    name = "pointers3_test",
    binary = ":pointers3",
    exit_code = 0,
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

test_suite(
    name = "package_tests",
    testonly = 0,
)

filegroup(
    name = "package_binaries",
    srcs = [],
)
