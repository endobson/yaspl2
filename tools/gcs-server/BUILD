load("//libraries:yaspl.bzl", "yaspl_binary")
load(
    "@io_bazel_rules_docker//container:container.bzl",
    "container_image",
    "container_push",
)

package(
    default_visibility = ["//visibility:public"],
)

yaspl_binary(
    name = "gcs-server",
    srcs = ["gcs-server-main.yaspl"],
    deps = [
        "//libraries:http-client_mainlib",
        "//libraries:http-server",
        "//libraries:io",
        "//libraries:ip",
        "//libraries:tcp",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:join-list",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:tuples",
        "//libraries/formats:html",
        "//libraries/formats:xml",
        "//libraries/protocols/http:http-response",
        "//libraries/protocols/http:http-responses",
    ],
)

container_image(
    name = "gcs-server-image",
    data_path = ".",
    entrypoint = [
        "/gcs-server",
        "0.0.0.0",
        "8080",
        "instance",
    ],
    files = [
        ":gcs-server",
    ],
)

container_push(
    name = "gcs-server-push",
    format = "Docker",
    image = ":gcs-server-image",
    registry = "gcr.io",
    repository = "crucial-bloom-404/gcs-server",
)

test_suite(
    name = "package_tests",
    testonly = 0,
)

filegroup(
    name = "package_binaries",
    srcs = [
        ":gcs-server",
    ],
)
