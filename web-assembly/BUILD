load(":wasm.bzl", "wasm_demo_page")
load("//libraries:yaspl.bzl", "yaspl_binary")

package(
    default_visibility = ["//visibility:public"],
)

genrule(
    name = "test_gen_wasm",
    outs = ["test.wasm"],
    cmd = "$(location :web-assembly-test) > $@",
    exec_tools = [":web-assembly-test"],
)

wasm_demo_page(
    name = "test",
    wasm_file = "test.wasm",
)

yaspl_binary(
    name = "web-assembly-test",
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
