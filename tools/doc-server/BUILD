load("@rules_oci//oci:defs.bzl", "oci_image", "oci_load", "oci_push")
load("@rules_pkg//pkg:pkg.bzl", "pkg_tar")
load("//libraries:yaspl.bzl", "yaspl_binary")
load("//tools:yaspl-source-files.bzl", "yaspl_source_files_rule")

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
        "//libraries/yaspl:parser",
        "//libraries/yaspl:top-level-objects",
        "//libraries/yaspl:top-level-objects-to-x86-64",
        "//libraries/yaspl:x86-64-assembly",
        "//libraries/yaspl/frontend:errors",
    ],
)

yaspl_source_files_rule(
    name = "source_files",
    deps = [
        "//libraries/data:dict",
    ],
)

pkg_tar(
    name = "doc-server-layer",
    srcs = [
        ":doc-server",
        ":source_files",
    ],
    strip_prefix = ".",
)

oci_image(
    name = "doc-server-image",
    architecture = "amd64",
    entrypoint = [
        "/doc-server",
        "0.0.0.0",
        "8080",
        "/source_files.list",
    ],
    os = "linux",
    tars = ["doc-server-layer"],
)

oci_push(
    name = "doc-server-push",
    image = ":doc-server-image",
    repository = "gcr.io/crucial-bloom-404/doc-server",
)

oci_load(
    name = "doc-server-load",
    image = ":doc-server-image",
    repo_tags = ":doc-server-repo-tags.txt",
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
