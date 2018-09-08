load("//libraries:yaspl.bzl", "yaspl_binary")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_binary(
    name = "hello-world",
    srcs = ["hello-world.yaspl"],
    deps = [],
)

yaspl_binary(
    name = "elf-file",
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
    srcs = ["env.yaspl"],
    deps = [
        "//libraries:io",
        "//libraries/data:list",
    ],
)

yaspl_binary(
    name = "http-echo-server",
    srcs = ["http-echo-server.yaspl"],
    deps = [
        "//libraries:http-server",
        "//libraries:io",
        "//libraries:ip",
        "//libraries:tcp",
        "//libraries/data:bytes",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
    ],
)

yaspl_binary(
    name = "thread-example",
    srcs = ["thread-example.yaspl"],
    deps = [
        "//libraries:io",
        "//libraries:mach",
    ],
)

yaspl_binary(
    name = "true",
    srcs = ["true.yaspl"],
    deps = [],
)

yaspl_binary(
    name = "wc",
    srcs = ["wc.yaspl"],
    deps = [
        "//libraries/data:numbers",
    ],
)

yaspl_binary(
    name = "udp-echo-server",
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
        ":http-echo-server",
        ":thread-example",
        ":true",
        ":udp-echo-server",
        ":wc",
    ],
)
