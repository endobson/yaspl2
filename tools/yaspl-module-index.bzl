load("//libraries:yaspl.bzl", "yaspl_provider")
load(":yaspl-transitive-rules.bzl", "transitivify_impl", "transitive_attrs",
     "concat_files_impl")

yaspl_module_index_provider = provider(fields=["files"])

def _yaspl_library_module_index_impl(target, ctx):
  output_file = ctx.actions.declare_file(ctx.label.name + ".module_index_part")
  target_yaspl_module_name_file = target[yaspl_provider].module_name_file
  ctx.actions.run_shell(
     outputs = [output_file],
     inputs = [target_yaspl_module_name_file],
     command = 'echo "#\\"%s\\"" >%s; cat %s >>%s'
         % (ctx.label, output_file.path,
            target_yaspl_module_name_file.path, output_file.path),
  )

  return yaspl_module_index_provider(files=depset([output_file]))

def _yaspl_module_index_impl(target, ctx):
  return [transitivify_impl(target, ctx, _yaspl_library_module_index_impl, yaspl_module_index_provider)]

yaspl_module_index = aspect(
  implementation = _yaspl_module_index_impl,
  attr_aspects = transitive_attrs,
  provides = [yaspl_module_index_provider],
)

def _yaspl_module_index_rule_impl(ctx):
  return concat_files_impl(ctx, yaspl_module_index_provider)

yaspl_module_index_rule = rule(
  implementation = _yaspl_module_index_rule_impl,
  attrs = {
    "deps": attr.label_list(aspects=[yaspl_module_index]),
    "_validator": attr.label(
       default=Label("//prebuilt:module-index-validator"),
       executable=True,
       allow_files=True,
       cfg="host",
    ),
  },
  outputs = {
    "combined": "%{name}.module_index"
  },
)
