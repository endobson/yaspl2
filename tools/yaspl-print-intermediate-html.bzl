load("//libraries:yaspl.bzl", "yaspl_provider")

def _yaspl_print_intermediate_html_impl(target, ctx):
  # TODO use required_aspect_providers once it works
  if ctx.rule.kind != "yaspl_library":
    return struct()

  html = ctx.actions.declare_file(target.label.name + ".inter.html")
  src_file = target[yaspl_provider].source_file

  input_signatures = target[yaspl_provider].input_signatures
  input_signature_paths = [sig.path for sig in input_signatures]

  ctx.actions.run(
    inputs = [src_file] + input_signatures,
    outputs = [html],
    executable = ctx.executable._intermediate_html_printer,
    arguments = [html.path, src_file.path] + input_signature_paths
  )

  return [
    OutputGroupInfo(intermediate_html = depset([html]))
  ]

yaspl_print_intermediate_html = aspect(
  implementation = _yaspl_print_intermediate_html_impl,
  attr_aspects = [],
  attrs = {
    "_intermediate_html_printer": attr.label(
       default=Label("//tools:intermediate-html-printer"),
       executable=True,
       allow_files=True,
       cfg="host",
    )
  }
)
