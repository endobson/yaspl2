load("//libraries:yaspl.bzl", "yaspl_binary")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_binary(
    name = "server",
    srcs = ["server-main.yaspl"],
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
        "//libraries/data:tuples",
        "//libraries/protocols/http:http-response",
        "//libraries/protocols/http:http-responses",
    ],
)

test_suite(
    name = "package_tests",
    testonly = 0,
)

filegroup(
    name = "package_binaries",
    srcs = [
        ":server",
    ],
)
