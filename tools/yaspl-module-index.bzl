load("//libraries:yaspl.bzl", "yaspl_provider")

yaspl_module_index_provider = provider(fields=["files"])

def _yaspl_library_module_index_impl(target, ctx):
  output_file = ctx.actions.declare_file(ctx.label.name + ".module_index_part")
  target_yaspl_provider = target[yaspl_provider]
  ctx.actions.run_shell(
     outputs = [output_file],
     inputs = [target_yaspl_provider.source_file],
     command = 'echo "#\\"%s\\"" "$(head -n 1 %s | sed -e "s/#:module //")" > %s'
         % (ctx.label, target_yaspl_provider.source_file.path, output_file.path),
  )
  local_provider = yaspl_module_index_provider(files=depset([output_file]))
  
  return [_merge_providers([local_provider] +_extract_providers(ctx.rule.attr.deps))]

def _yaspl_binary_module_index_impl(target, ctx):
  return [_merge_providers(_extract_providers(ctx.rule.attr.deps))]

def _test_suite_module_index_impl(target, ctx):
  return [_merge_providers(_extract_providers(ctx.rule.attr.tests))]

def _filegroup_module_index_impl(target, ctx):
  return [_merge_providers(_extract_providers(ctx.rule.attr.srcs))]

def _merge_providers(providers):
  files = depset(transitive=[p.files for p in providers])
  return yaspl_module_index_provider(files = files)
def _extract_providers(objs):
  return [obj[yaspl_module_index_provider] for obj in objs]

def _yaspl_module_index_impl(target, ctx):
  kind = ctx.rule.kind 
  if (kind == "yaspl_library"):
    return _yaspl_library_module_index_impl(target, ctx)
  elif (kind == "yaspl_binary"):
    return _yaspl_binary_module_index_impl(target, ctx)
  elif (kind == "test_suite"):
    return _test_suite_module_index_impl(target, ctx)
  elif (kind == "filegroup"):
    return _filegroup_module_index_impl(target, ctx)
  else:
    fail("Unknown rule kind")

yaspl_module_index = aspect(
  implementation = _yaspl_module_index_impl,
  attr_aspects = ["tests", "srcs", "deps"],
  provides = [yaspl_module_index_provider],
)

def _yaspl_module_index_rule_impl(ctx):
  args = ctx.actions.args()
  index_file_parts = depset(transitive=[d[yaspl_module_index_provider].files for d in ctx.attr.deps])
  args.add_all(index_file_parts)
  ctx.actions.run_shell(
     outputs = [ctx.outputs.index],
     inputs = index_file_parts,
     command = "cat >%s $@" % ctx.outputs.index.path,
     arguments = [args]
  )
  return []

yaspl_module_index_rule = rule(
  implementation = _yaspl_module_index_rule_impl,
  attrs = {
    "deps": attr.label_list(aspects=[yaspl_module_index])
  },
  outputs = {
    "index": "%{name}.module_index"
  },
)
