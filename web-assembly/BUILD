load(":wasm.bzl", "wasm_demo_page")

genrule(
   name = "test_gen_wasm",
   outs = ["test.wasm"],
   srcs = ["test.wasm.hex"],
   cmd = "xxd -r -p $< > $@"
)

wasm_demo_page(
  name = "test",
  wasm_file = "test.wasm",
)
