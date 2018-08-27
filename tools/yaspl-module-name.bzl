load("//libraries:yaspl.bzl", "yaspl_provider")

yaspl_module_name_provider = provider(fields=["file"])

def _yaspl_library_module_name_impl(target, ctx):
  output_file = ctx.actions.declare_file(ctx.label.name + ".module_name")
  target_yaspl_provider = target[yaspl_provider]
  ctx.actions.run_shell(
     outputs = [output_file],
     inputs = [target_yaspl_provider.source_file],
     command = 'echo "$(head -n 1 %s | sed -e "s/#:module //")" > %s'
         % (target_yaspl_provider.source_file.path, output_file.path),
  )
  return [yaspl_module_name_provider(file=output_file)]

def _yaspl_module_name_impl(target, ctx):
  kind = ctx.rule.kind
  if (kind == "yaspl_library"):
    return _yaspl_library_module_name_impl(target, ctx)
  elif (kind == "yaspl_binary"):
    return [yaspl_module_name_provider(file=False)]
  elif (kind == "test_suite"):
    return [yaspl_module_name_provider(file=False)]
  elif (kind == "filegroup"):
    return [yaspl_module_name_provider(file=False)]
  else:
    fail("Unknown rule kind")

yaspl_module_name = aspect(
  implementation = _yaspl_module_name_impl,
  attr_aspects = ["tests", "srcs", "deps"],
  provides = [yaspl_module_name_provider],
)
