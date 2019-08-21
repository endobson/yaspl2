load("//libraries:yaspl.bzl", "yaspl_provider")
load(":yaspl-transitive-rules.bzl", "transitivify_impl", "transitive_attrs",
     "concat_files_impl")

yaspl_exported_values_provider = provider(fields=["files"])

def _yaspl_library_exported_values_impl(target, ctx):
  output_file = ctx.actions.declare_file(ctx.label.name + ".exported_values")
  target_yaspl_provider = target[yaspl_provider]

  ctx.actions.run_shell(
     outputs = [output_file],
     inputs = [target_yaspl_provider.source_file],
     tools = [ctx.executable._exported_values],
     command = '%s %s >%s' %
         (ctx.executable._exported_values.path,
          target_yaspl_provider.source_file.path,
          output_file.path),
  )
  return yaspl_exported_values_provider(files=depset([output_file]))


def _yaspl_exported_values_impl(target, ctx):
  return [transitivify_impl(target, ctx, _yaspl_library_exported_values_impl, yaspl_exported_values_provider)]

yaspl_exported_values = aspect(
  implementation = _yaspl_exported_values_impl,
  attr_aspects = transitive_attrs,
  provides = [yaspl_exported_values_provider],
  attrs = {
    "_exported_values": attr.label(
       default=Label("//tools:aspect-exported-values"),
       executable=True,
       allow_files=True,
       cfg="host",
    ),
  },
)

def _yaspl_exported_values_rule_impl(ctx):
  return concat_files_impl(ctx, yaspl_exported_values_provider)

yaspl_exported_values_rule = rule(
  implementation = _yaspl_exported_values_rule_impl,
  attrs = {
    "deps": attr.label_list(aspects=[yaspl_exported_values]),
  },
  outputs = {
    "combined": "%{name}.exported_values"
  },
)
