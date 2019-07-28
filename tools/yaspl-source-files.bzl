load("//libraries:yaspl.bzl", "yaspl_provider")
load(":yaspl-transitive-rules.bzl", "transitivify_impl", "transitive_attrs")

yaspl_source_files_provider = provider(fields=["files"])

def _yaspl_library_source_files_impl(target, ctx):
  return yaspl_source_files_provider(files=depset([target[yaspl_provider].source_file]))

def _yaspl_source_files_impl(target, ctx):
  return [transitivify_impl(target, ctx, _yaspl_library_source_files_impl, yaspl_source_files_provider)]

yaspl_source_files = aspect(
  implementation = _yaspl_source_files_impl,
  attr_aspects = transitive_attrs,
  provides = [yaspl_source_files_provider],
)

def _yaspl_source_files_rule_impl(ctx):
  output_list = ctx.actions.declare_file("%s.list" % ctx.attr.name)

  source_files = depset(transitive=[d[yaspl_source_files_provider].files for d in ctx.attr.deps])

  args = ctx.actions.args()
  args.add_all(source_files)

  ctx.actions.write(
    output=output_list,
    content=args
  )

  return [
    DefaultInfo(
      files = depset(direct = [output_list], transitive = [source_files]),
    ),
  ]

yaspl_source_files_rule = rule(
  implementation = _yaspl_source_files_rule_impl,
  attrs = {
    "deps": attr.label_list(aspects=[yaspl_source_files]),
  },
)
