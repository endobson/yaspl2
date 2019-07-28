def _wasm_demo_page_impl(ctx):
  output_html = ctx.actions.declare_file("%s/index.html" % ctx.attr.name)
  output_wasm = ctx.actions.declare_file("%s/data.wasm" % ctx.attr.name)

  html_template_file = ctx.attr._html_template.files.to_list()[0]
  ctx.actions.expand_template(
     template = html_template_file,
     output = output_html,
     substitutions = {}
  )

  input_wasm_file = ctx.attr.wasm_file.files.to_list()[0];
  ctx.actions.run_shell(
    inputs = [input_wasm_file],
    outputs = [output_wasm],
    mnemonic = "WasmDemoCopyWasm",
    command = "cp " + input_wasm_file.path + " " + output_wasm.path
  )

  return [
    DefaultInfo(
      files = depset([output_wasm, output_html])
    ),
  ]

wasm_demo_page = rule(
  implementation = _wasm_demo_page_impl,
  attrs = {
    "wasm_file": attr.label(allow_single_file=[".wasm"]),
    "_html_template": attr.label(
      default=Label("//web-assembly:host.html"),
      allow_files=True,
    )
  }
)
