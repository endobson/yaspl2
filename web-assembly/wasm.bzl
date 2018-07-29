def _wasm_demo_page_impl(ctx):
  html_template_file = ctx.attr._html_template.files.to_list()[0]
  ctx.actions.expand_template(
     template = html_template_file,
     output = ctx.outputs.html,
     substitutions = {}
  )

  input_wasm_file = ctx.attr.wasm_file.files.to_list()[0];
  ctx.actions.run_shell(
    inputs = [input_wasm_file],
    outputs = [ctx.outputs.wasm],
    mnemonic = "WasmDemoCopyWasm",
    command = "cp " + input_wasm_file.path + " " + ctx.outputs.wasm.path
  )

wasm_demo_page = rule(
  implementation = _wasm_demo_page_impl,
  outputs = {
    "html": "%{name}/index.html",
    "wasm": "%{name}/data.wasm",
  },
  attrs = {
    "wasm_file": attr.label(allow_single_file=[".wasm"]),
    "_html_template": attr.label(
      default=Label("//web-assembly:host.html"),
      allow_files=True,
    )
  }
)
