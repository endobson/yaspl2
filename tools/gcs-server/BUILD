load("@rules_oci//oci:defs.bzl", "oci_image", "oci_push")
load("@rules_pkg//pkg:pkg.bzl", "pkg_tar")
load("//libraries:yaspl.bzl", "yaspl_binary")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_binary(
    name = "gcs-server",
    srcs = ["gcs-server-main.yaspl"],
    deps = [
        "//libraries:http-server",
        "//libraries:io",
        "//libraries:ip",
        "//libraries:tcp",
        "//libraries/data:bytes",
        "//libraries/data:join-list",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/formats:html",
        "//libraries/formats:xml",
        "//libraries/protocols/http:http-response",
        "//libraries/protocols/http:http-responses",
    ],
)

pkg_tar(
    name = "gcs-server-layer",
    srcs = [
        ":gcs-server",
    ],
)

oci_image(
    name = "gcs-server-image",
    architecture = "amd64",
    entrypoint = [
        "/gcs-server",
        "0.0.0.0",
        "8080",
        "instance",
    ],
    os = "linux",
    tars = [
        ":gcs-server-layer",
    ],
)

oci_push(
    name = "gcs-server-push",
    image = ":gcs-server-image",
    repository = "gcr.io/crucial-bloom-404/gcs-server",
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
