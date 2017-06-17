load("//libraries:yaspl.bzl", "yaspl_library", "yaspl_binary")

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
        "//libraries/data:list",
        "//libraries/data:maybe",
    ],
)
