load("//libraries:yaspl.bzl", "yaspl_provider")
load(":yaspl-transitive-rules.bzl", "transitivify_impl", "transitive_attrs",
     "concat_files_impl", "concat_attrs")

yaspl_lint_provider = provider(fields=["files"])

def _yaspl_library_lint_impl(target, ctx):
  output = ctx.actions.declare_file(ctx.label.name + ".lint")
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
    tools = [ctx.executable._linter],
    arguments = [args],
  )

  return yaspl_lint_provider(files=depset([output]))

def _yaspl_lint_impl(target, ctx):
  return [transitivify_impl(target, ctx, _yaspl_library_lint_impl, yaspl_lint_provider)]

yaspl_lint = aspect(
  implementation = _yaspl_lint_impl,
  attr_aspects = transitive_attrs,
  provides = [yaspl_lint_provider],
  attrs = {
    "_linter": attr.label(
       default=Label("//tools:aspect-linter"),
       executable=True,
       cfg="host",
    )
  }
)

def _yaspl_lint_rule_impl(ctx):
  return concat_files_impl(ctx, yaspl_lint_provider)

yaspl_lint_rule = rule(
  implementation = _yaspl_lint_rule_impl,
  attrs = concat_attrs(yaspl_lint),
  outputs = {
    "combined": "%{name}.lint"
  },
)
