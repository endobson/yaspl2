def _yaspl_lint_impl(target, ctx):
  if ctx.rule.kind != "yaspl_library":
    return struct()

  paired_outputs = [(src, ctx.new_file(src.label.name + ".lint")) for src in ctx.rule.attr.srcs]
  for (src, output) in paired_outputs:
    if len(src.files) != 1:
      fail("YasplLint only works on yasple libraries with one source file")
    src_file = src.files.to_list()[0]
    ctx.action(
      inputs = [src_file] + [dep.yaspl_signature for dep in ctx.rule.attr.deps],
      outputs = [output],
      executable = ctx.executable._linter,
      arguments = [src_file.path, output.path] +
                  [dep.yaspl_signature.path for dep in ctx.rule.attr.deps]
    )

  return struct(
    output_groups = {
      "lint": [output for (src, output) in paired_outputs]
    }
  )

yaspl_lint = aspect(
  implementation = _yaspl_lint_impl,
  attr_aspects = [],
  attrs = {
    "_linter": attr.label(
       default=Label("//tools:aspect-linter"),
       executable=True,
       allow_files=True,
       cfg="host",
    )
  }
)
