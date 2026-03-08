load("@rules_oci//oci:defs.bzl", "oci_image", "oci_push")
load("@rules_pkg//pkg:pkg.bzl", "pkg_tar")
load("//libraries:yaspl.bzl", "yaspl_binary", "yaspl_library")
load("//rules:yaspl-embed-file.bzl", "yaspl_embed_file")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_library(
    name = "client-code",
    srcs = ["client-code.yaspl"],
    deps = [
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:either",
        "//libraries/data:join-list",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:tuples",
        "//libraries/formats:web-assembly",
    ],
)

yaspl_binary(
    name = "web-calculator",
    srcs = ["web-calculator-main.yaspl"],
    deps = [
        ":client-code",
        ":web-calculator-embed",
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
        "//libraries/formats:html",
        "//libraries/formats:web-assembly",
        "//libraries/formats:xml",
        "//libraries/protocols/http:http-response",
        "//libraries/protocols/http:http-responses",
    ],
)

yaspl_embed_file(
    name = "web-calculator-embed",
    srcs = ["web-calculator.js"],
    module_name = "tools web-calculator embed",
)

yaspl_binary(
    name = "wasm-generator",
    srcs = ["wasm-generator-main.yaspl"],
    deps = [
        "//libraries:io",
        "//libraries:sexp-parser",
        "//libraries/data:either",
        "//libraries/data:list",
        "//libraries/data:source-location",
        "//libraries/formats:sexp",
        "//libraries/formats:web-assembly",
    ],
)

pkg_tar(
    name = "web-calculator-layer",
    srcs = [
        ":web-calculator",
    ],
)

oci_image(
    name = "web-calculator-image",
    architecture = "amd64",
    entrypoint = [
        "/web-calculator",
        "0.0.0.0",
        "8080",
    ],
    os = "linux",
    tars = [
        ":web-calculator-layer",
    ],
)

oci_push(
    name = "web-calculator-push",
    image = ":web-calculator-image",
    repository = "gcr.io/crucial-bloom-404/web-calculator",
)

test_suite(
    name = "package_tests",
    testonly = 0,
)

filegroup(
    name = "package_binaries",
    srcs = [
        ":wasm-generator",
        ":web-calculator",
    ],
)
