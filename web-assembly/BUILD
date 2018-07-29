load(":wasm.bzl", "wasm_demo_page")
load("//libraries:yaspl.bzl", "yaspl_binary", "yaspl_library")

genrule(
   name = "test_gen_wasm",
   outs = ["test.wasm"],
   tools = [":web-assembly-test"],
   cmd = "$(location :web-assembly-test) > $@"
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
    "//libraries/data:list",
    "//libraries/formats:web-assembly",
    "//libraries:io",
  ]
)
