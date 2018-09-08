load("//libraries:yaspl.bzl", "yaspl_provider")

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
  local_provider = yaspl_missing_imports_provider(files=depset([output_file]))

  return [_merge_providers([local_provider] +_extract_providers(ctx.rule.attr.deps))]


def _yaspl_binary_missing_imports_impl(target, ctx):
  return [_merge_providers(_extract_providers(ctx.rule.attr.deps))]

def _test_suite_missing_imports_impl(target, ctx):
  return [_merge_providers(_extract_providers(ctx.rule.attr.tests))]

def _filegroup_missing_imports_impl(target, ctx):
  return [_merge_providers(_extract_providers(ctx.rule.attr.srcs))]

def _merge_providers(providers):
  files = depset(transitive=[p.files for p in providers])
  return yaspl_missing_imports_provider(files = files)
def _extract_providers(objs):
  return [obj[yaspl_missing_imports_provider] for obj in objs]

def _yaspl_missing_imports_impl(target, ctx):
  kind = ctx.rule.kind 
  if (kind == "yaspl_library"):
    return _yaspl_library_missing_imports_impl(target, ctx)
  elif (kind == "yaspl_binary"):
    return _yaspl_binary_missing_imports_impl(target, ctx)
  elif (kind == "test_suite"):
    return _test_suite_missing_imports_impl(target, ctx)
  elif (kind == "filegroup"):
    return _filegroup_missing_imports_impl(target, ctx)
  else:
    fail("Unknown rule kind")

yaspl_missing_imports = aspect(
  implementation = _yaspl_missing_imports_impl,
  attr_aspects = ["tests", "srcs", "deps"],
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
  args = ctx.actions.args()
  index_file_parts = depset(transitive=[d[yaspl_missing_imports_provider].files for d in ctx.attr.deps])
  args.add_all(index_file_parts)
  ctx.actions.run_shell(
     outputs = [ctx.outputs.index],
     inputs = index_file_parts,
     command = "cat >%s $@" % ctx.outputs.index.path,
     arguments = [args]
  )
  return []

yaspl_missing_imports_rule = rule(
  implementation = _yaspl_missing_imports_rule_impl,
  attrs = {
    "deps": attr.label_list(aspects=[yaspl_missing_imports])
  },
  outputs = {
    "index": "%{name}.missing_imports"
  },
)
