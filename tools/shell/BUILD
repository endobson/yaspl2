load("//libraries:yaspl.bzl", "yaspl_binary", "yaspl_library")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_binary(
    name = "shell",
    main_module = "tools_shell_shell_main",
    deps = [
        ":shell-main",
    ],
)

yaspl_library(
    name = "shell-main",
    srcs = ["shell-main.yaspl"],
    deps = [
        "//libraries:io",
        "//libraries/data:bytes",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
    ],
)

test_suite(
    name = "package_tests",
    testonly = 0,
)

filegroup(
    name = "package_binaries",
    srcs = [
        ":shell",
    ],
)
