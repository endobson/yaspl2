load("//libraries:yaspl.bzl", "yaspl_provider")

def _yaspl_print_ir_impl(target, ctx):
  if ctx.rule.kind != "yaspl_library":
    return struct()

  directory = ctx.actions.declare_directory(target.label.name + ".ir")
  src_file = target[yaspl_provider].source_file
  input_signatures = target[yaspl_provider].input_signatures

  args = ctx.actions.args()
  args.add(directory.path)
  args.add(src_file)
  args.add_all(input_signatures)

  ctx.actions.run(
    inputs = [src_file] + input_signatures,
    outputs = [directory],
    executable = ctx.executable._ir_printer,
    arguments = [args]
  )

  return [
    OutputGroupInfo(ir = depset([directory]))
  ]

yaspl_print_ir = aspect(
  implementation = _yaspl_print_ir_impl,
  attrs = {
    "_ir_printer": attr.label(
       default=Label("//tools:ir-printer"),
       executable=True,
       cfg="host",
    )
  }
)
