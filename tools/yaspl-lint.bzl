load("//libraries:yaspl.bzl", "yaspl_provider")

def _yaspl_lint_impl(target, ctx):
  if ctx.rule.kind != "yaspl_library":
    return struct()

  output = ctx.new_file(ctx.label.name + ".lint")
  src_file = target[yaspl_provider].source_file
  input_signatures = target[yaspl_provider].input_signatures

  args = ctx.actions.args()
  args.add(src_file)
  args.add(output)
  args.add_all(input_signatures)

  ctx.actions.run(
    inputs = [src_file] + input_signatures,
    outputs = [output],
    executable = ctx.executable._linter,
    arguments = [args],
  )

  return [
    OutputGroupInfo(lint = [output])
  ]

yaspl_lint = aspect(
  implementation = _yaspl_lint_impl,
  attrs = {
    "_linter": attr.label(
       default=Label("//tools:aspect-linter"),
       executable=True,
       cfg="host",
    )
  }
)
