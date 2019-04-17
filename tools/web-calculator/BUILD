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
    name = "web-calculator",
    srcs = ["web-calculator-main.yaspl"],
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
    ],
)

container_image(
    name = "web-calculator-image",
    files = [
        ":web-calculator",
    ],
    entrypoint = ["/web-calculator", "0.0.0.0", "8080"],
)

container_push(
   name = "web-calculator-push",
   image = ":web-calculator-image",
   format = "Docker",
   registry = "gcr.io",
   repository = "crucial-bloom-404/web-calculator",
)

test_suite(
    name = "package_tests",
    testonly = 0,
)

filegroup(
    name = "package_binaries",
    srcs = [
        ":web-calculator",
    ],
)
