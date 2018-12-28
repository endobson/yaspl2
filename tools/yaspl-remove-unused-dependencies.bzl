load("//libraries:yaspl.bzl", "yaspl_provider")
load(":yaspl-transitive-rules.bzl", "transitivify_impl", "transitive_attrs",
     "concat_files_impl", "concat_attrs")

yaspl_remove_unused_dependencies_provider = provider(fields=["files"])

def _yaspl_library_remove_unused_dependencies_impl(target, ctx):
  output_file = ctx.actions.declare_file(ctx.label.name + ".remove_unused_dependencies_part")
  target_yaspl_provider = target[yaspl_provider]
  deps_module_name_files = [dep[yaspl_provider].module_name_file for dep in ctx.rule.attr.deps]
  source_rule_name = ctx.rule.attr.source_rule_name
  label = ctx.label.relative(source_rule_name) if source_rule_name else ctx.label

  ctx.actions.run_shell(
     outputs = [output_file],
     inputs = [target_yaspl_provider.source_file, ctx.file._module_index]
              + deps_module_name_files,
     tools = [ctx.executable._remove_unused_dependencies],
     command = '%s %s %s %s $@ >%s' %
         (ctx.executable._remove_unused_dependencies.path,
          label,
          ctx.file._module_index.path,
          target_yaspl_provider.source_file.path,
          output_file.path),
     arguments = [file.path for file in deps_module_name_files],
  )
  return yaspl_remove_unused_dependencies_provider(files=depset([output_file]))

def _yaspl_remove_unused_dependencies_impl(target, ctx):
  return [transitivify_impl(target, ctx, _yaspl_library_remove_unused_dependencies_impl,
                            yaspl_remove_unused_dependencies_provider)]

yaspl_remove_unused_dependencies = aspect(
  implementation = _yaspl_remove_unused_dependencies_impl,
  attr_aspects = transitive_attrs,
  provides = [yaspl_remove_unused_dependencies_provider],
  attrs = {
    "_remove_unused_dependencies": attr.label(
       default=Label("//tools:aspect-remove-unused-dependencies"),
       executable=True,
       allow_files=True,
       cfg="host",
    ),
    "_module_index": attr.label(
       default=Label("//:module_index"),
       allow_single_file=True,
    ),
  }
)

def _yaspl_remove_unused_dependencies_rule_impl(ctx):
  return concat_files_impl(ctx, yaspl_remove_unused_dependencies_provider)

yaspl_remove_unused_dependencies_rule = rule(
  implementation = _yaspl_remove_unused_dependencies_rule_impl,
  attrs = concat_attrs(yaspl_remove_unused_dependencies),
  outputs = {
    "combined": "%{name}.remove_unused_dependencies"
  },
)
