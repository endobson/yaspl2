load("//libraries:yaspl.bzl", "yaspl_binary")
load("//tools:yaspl-source-files.bzl", "yaspl_source_files_rule")
load(
    "@io_bazel_rules_docker//container:container.bzl",
    "container_image",
    "container_push",
)

package(
    default_visibility = ["//visibility:public"],
)

yaspl_binary(
    name = "doc-server",
    srcs = ["doc-server-main.yaspl"],
    deps = [
        "//libraries:compiler_lib",
        "//libraries:http-server",
        "//libraries:io",
        "//libraries:ip",
        "//libraries:tcp",
        "//libraries/code-http-server:assembly",
        "//libraries/code-http-server:register",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:join-list",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/formats:html",
        "//libraries/formats:xml",
        "//libraries/protocols/http:http-response",
        "//libraries/protocols/http:http-responses",
        "//libraries/yaspl:top-level-objects",
        "//libraries/yaspl:top-level-objects-to-x86-64",
        "//libraries/yaspl:x86-64-assembly",
    ],
)

yaspl_source_files_rule(
    name = "source_files",
    deps = [
        "//libraries/data:dict",
    ],
)

container_image(
    name = "doc-server-image",
    data_path = ".",
    entrypoint = [
        "/doc-server",
        "0.0.0.0",
        "8080",
        "/source_files.list",
    ],
    files = [
        ":doc-server",
        ":source_files",
        ":source_files.list",
    ],
)

container_push(
    name = "doc-server-push",
    format = "Docker",
    image = ":doc-server-image",
    registry = "gcr.io",
    repository = "crucial-bloom-404/doc-server",
)

test_suite(
    name = "package_tests",
    testonly = 0,
)

filegroup(
    name = "package_binaries",
    srcs = [
        ":doc-server",
    ],
)
