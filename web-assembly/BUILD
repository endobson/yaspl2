load(":wasm.bzl", "wasm_demo_page")
load("//libraries:yaspl.bzl", "yaspl_binary", "yaspl_library")

package(
    default_visibility = ["//visibility:public"],
)

genrule(
    name = "test_gen_wasm",
    outs = ["test.wasm"],
    cmd = "$(location :web-assembly-test) > $@",
    tools = [":web-assembly-test"],
)

wasm_demo_page(
    name = "test",
    wasm_file = "test.wasm",
)

yaspl_binary(
    name = "web-assembly-test",
    main_module = "web_assembly_test",
    deps = [":web-assembly-test_lib"],
)

yaspl_library(
    name = "web-assembly-test_lib",
    srcs = ["test.yaspl"],
    deps = [
        "//libraries:io",
        "//libraries/data:list",
        "//libraries/formats:web-assembly",
    ],
)

test_suite(
    name = "package_tests",
    testonly = 0,
)

filegroup(
    name = "package_binaries",
    srcs = [
        ":web-assembly-test",
    ],
)
