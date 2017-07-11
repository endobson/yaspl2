load("//libraries:yaspl.bzl", "yaspl_provider")

def _yaspl_print_ir_impl(target, ctx):
  # TODO use required_aspect_providers once it works
  if ctx.rule.kind != "yaspl_library":
    return struct()

  directory = ctx.actions.declare_directory(target.label.name + ".ir")
  src_file = target[yaspl_provider].source_file

  ctx.actions.run(
    inputs = [],
    outputs = [directory],
    executable = ctx.executable._ir_printer,
    arguments = [src_file.path, directory.path]
  )


  return [
    OutputGroupInfo(ir = depset([directory]))
  ]

  ## paired_outputs = [(src, ctx.new_file(src.label.name + ".lint")) for src in ctx.rule.attr.srcs]
  ## for (src, output) in paired_outputs:
  ##   if len(src.files) != 1:
  ##     fail("YasplLint only works on yasple libraries with one source file")
  ##   src_file = src.files.to_list()[0]
  ##   ctx.action(
  ##     inputs = [src_file] + [dep.yaspl_signature for dep in ctx.rule.attr.deps],
  ##     outputs = [output],
  ##     executable = ctx.executable._linter,
  ##     arguments = [src_file.path, output.path] +
  ##                 [dep.yaspl_signature.path for dep in ctx.rule.attr.deps]
  ##   )

  ## return struct(
  ##   output_groups = {
  ##     "lint": [output for (src, output) in paired_outputs]
  ##   }
  ## )



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
