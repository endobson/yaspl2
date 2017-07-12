load("//libraries:yaspl.bzl", "yaspl_provider")

def _yaspl_print_ir_impl(target, ctx):
  # TODO use required_aspect_providers once it works
  if ctx.rule.kind != "yaspl_library":
    return struct()

  directory = ctx.actions.declare_directory(target.label.name + ".ir")
  src_file = target[yaspl_provider].source_file

  input_signatures = target[yaspl_provider].input_signatures
  input_signature_paths = [sig.path for sig in input_signatures]

  ctx.actions.run(
    inputs = [src_file] + input_signatures,
    outputs = [directory],
    executable = ctx.executable._ir_printer,
    arguments = [src_file.path, directory.path] + input_signature_paths
  )


  return [
    OutputGroupInfo(ir = depset([directory]))
  ]

yaspl_print_ir = aspect(
  implementation = _yaspl_print_ir_impl,
  attr_aspects = [],
  attrs = {
    "_ir_printer": attr.label(
       default=Label("//tools:ir-printer"),
       executable=True,
       allow_files=True,
       cfg="host",
    )
  }
)
