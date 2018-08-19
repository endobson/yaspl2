load("//libraries:yaspl.bzl", "yaspl_binary", "yaspl_library")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_binary(
    name = "hello-world",
    main_module = "hello_world",
    deps = [":hello-world_lib"],
)

yaspl_library(
    name = "hello-world_lib",
    srcs = ["hello-world.yaspl"],
    deps = [],
)

yaspl_binary(
    name = "elf-file",
    main_module = "elf_file",
    deps = [":elf-file_lib"],
)

yaspl_library(
    name = "elf-file_lib",
    srcs = ["elf-file.yaspl"],
    deps = [
        "//libraries:io",
        "//libraries:structured-native-code",
        "//libraries/data:aligned-bytes",
        "//libraries/data:list",
        "//libraries/formats:elf",
    ],
)

yaspl_binary(
    name = "env",
    main_module = "env",
    deps = [":env_lib"],
)

yaspl_library(
    name = "env_lib",
    srcs = ["env.yaspl"],
    deps = [
        "//libraries:io",
        "//libraries/data:list",
    ],
)

yaspl_binary(
    name = "thread-example",
    main_module = "thread_example",
    deps = [":thread-example_lib"],
)

yaspl_library(
    name = "thread-example_lib",
    srcs = ["thread-example.yaspl"],
    deps = [
        "//libraries:io",
        "//libraries:mach",
    ],
)

yaspl_binary(
    name = "true",
    main_module = "true",
    deps = [":true_lib"],
)

yaspl_library(
    name = "true_lib",
    srcs = ["true.yaspl"],
    deps = [],
)

yaspl_binary(
    name = "wc",
    main_module = "wc",
    deps = [":wc_lib"],
)

yaspl_library(
    name = "wc_lib",
    srcs = ["wc.yaspl"],
    deps = [
        "//libraries/data:numbers",
    ],
)

yaspl_binary(
    name = "udp-echo-server",
    main_module = "udp_echo_server",
    deps = [":udp-echo-server_lib"],
)

yaspl_library(
    name = "udp-echo-server_lib",
    srcs = ["udp-echo-server.yaspl"],
    deps = [
        "//libraries:io",
        "//libraries:ip",
        "//libraries:udp",
        "//libraries/data:bytes",
        "//libraries/data:lifted-primitives",
        "//libraries/data:numbers",
        "//libraries/data:tuples",
    ],
)

test_suite(
    name = "package_tests",
    testonly = 0,
)

filegroup(
    name = "package_binaries",
    srcs = [
        ":elf-file",
        ":env",
        ":hello-world",
        ":thread-example",
        ":true",
        ":udp-echo-server",
        ":wc",
    ],
)
