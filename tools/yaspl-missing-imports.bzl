load("//libraries:yaspl.bzl", "yaspl_provider")
load(":yaspl-transitive-rules.bzl", "transitivify_impl", "transitive_attrs",
     "concat_files_impl", "concat_attrs")

yaspl_missing_imports_provider = provider(fields=["files"])

def _yaspl_library_missing_imports_impl(target, ctx):
  output_file = ctx.actions.declare_file(ctx.label.name + ".missing_imports_part")
  target_yaspl_provider = target[yaspl_provider]
  deps_module_name_files = [dep[yaspl_provider].module_name_file for dep in ctx.rule.attr.deps]

  ctx.actions.run_shell(
     outputs = [output_file],
     inputs = [target_yaspl_provider.source_file,  ctx.executable._missing_imports,
               ctx.file._module_index]
              + deps_module_name_files,
     command = '%s %s %s %s $@ >%s' %
         (ctx.executable._missing_imports.path, 
          ctx.label,
          ctx.file._module_index.path,
          target_yaspl_provider.source_file.path,
          output_file.path),
     arguments = [file.path for file in deps_module_name_files],
  )
  return yaspl_missing_imports_provider(files=depset([output_file]))

def _yaspl_missing_imports_impl(target, ctx):
  return [transitivify_impl(target, ctx, _yaspl_library_missing_imports_impl, yaspl_missing_imports_provider)]

yaspl_missing_imports = aspect(
  implementation = _yaspl_missing_imports_impl,
  attr_aspects = transitive_attrs,
  provides = [yaspl_missing_imports_provider],
  attrs = {
    "_missing_imports": attr.label(
       default=Label("//prebuilt:aspect-missing-imports"),
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

def _yaspl_missing_imports_rule_impl(ctx):
  return concat_files_impl(ctx, yaspl_missing_imports_provider)

yaspl_missing_imports_rule = rule(
  implementation = _yaspl_missing_imports_rule_impl,
  attrs = concat_attrs(yaspl_missing_imports),
  outputs = {
    "combined": "%{name}.missing_imports"
  },
)
