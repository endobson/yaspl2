load("//libraries:yaspl.bzl", "yaspl_provider")

def _yaspl_lint_impl(target, ctx):

  # TODO use required_aspect_providers once it works
  if ctx.rule.kind != "yaspl_library":
    return struct()

  src_file = target[yaspl_provider].source_file
  output = ctx.new_file(ctx.label.name + ".lint")
  ctx.action(
    inputs = [src_file] + [dep[yaspl_provider].signature for dep in ctx.rule.attr.deps],
    outputs = [output],
    executable = ctx.executable._linter,
    arguments = [src_file.path, output.path] +
                [dep[yaspl_provider].signature.path for dep in ctx.rule.attr.deps]
  )

  return struct(
    output_groups = {
      "lint": [output]
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
