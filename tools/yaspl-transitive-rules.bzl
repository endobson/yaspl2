def _merge_fields(ctx, fields, p):
  targets = []
  for field in fields:
    targets += getattr(ctx.rule.attr, field)
  return _merge_providers([t[p] for t in targets], p)

def _merge_providers(ps, p):
  return p(files=depset(transitive=[p.files for p in ps]))

def transitivify_impl(target, ctx, impl, p):
  kind = ctx.rule.kind
  if (kind == "yaspl_library"):
    return _merge_providers([_merge_fields(ctx, ["deps"], p), impl(target, ctx)], p)
  elif (kind == "yaspl_prim_binary"):
    return _merge_fields(ctx, ["deps"], p)
  elif (kind == "yaspl_prim_test"):
    return _merge_fields(ctx, ["deps"], p)
  elif (kind == "test_suite"):
    return _merge_fields(ctx, ["tests", "_implicit_tests"], p)
  elif (kind == "filegroup"):
    return _merge_fields(ctx, ["srcs"], p)
  elif (kind == "binary_test"):
    return _merge_providers([], p)
  else:
    fail("Unknown rule kind")

transitive_attrs = ["deps", "srcs", "tests", "_implicit_tests"]


def concat_files_impl(ctx, p):
  args = ctx.actions.args()
  file_parts = depset(transitive=[d[p].files for d in ctx.attr.deps])
  args.add_all(file_parts)

  if hasattr(ctx.executable, "_validator"):
    tools = [ctx.executable._validator]
    command = "cat >%s $@ && %s %s" % (
        ctx.outputs.combined.path,
        ctx.executable._validator.path,
        ctx.outputs.combined.path)
  else:
    tools = []
    command = "cat >%s $@" % ctx.outputs.combined.path

  ctx.actions.run_shell(
     outputs = [ctx.outputs.combined],
     inputs = file_parts,
     command = command,
     tools = tools,
     arguments = [args],
  )
  return []
