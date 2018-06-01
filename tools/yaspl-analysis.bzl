load("//libraries:yaspl.bzl", "yaspl_provider")

def _yaspl_analysis_impl(target, ctx):

  # TODO use required_aspect_providers once it works
  if ctx.rule.kind != "yaspl_library":
    return struct()

  input_signatures = target[yaspl_provider].input_signatures
  input_signature_paths = [sig.path for sig in input_signatures]

  src_file = target[yaspl_provider].source_file
  output = ctx.new_file(ctx.label.name + ".analysis")
  ctx.action(
    inputs = [src_file] + input_signatures,
    outputs = [output],
    executable = ctx.executable._analysis,
    arguments = [src_file.path, output.path] + input_signature_paths,
  )

  return [
    OutputGroupInfo(analysis = [output])
  ]

yaspl_analysis = aspect(
  implementation = _yaspl_analysis_impl,
  attr_aspects = [],
  attrs = {
    "_analysis": attr.label(
       default=Label("//tools:aspect-analysis"),
       executable=True,
       allow_files=True,
       cfg="host",
    )
  }
)
