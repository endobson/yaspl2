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
  elif (kind == "yaspl_binary"):
    return _merge_fields(ctx, ["deps"], p)
  elif (kind == "test_suite"):
    return _merge_fields(ctx, ["tests"], p)
  elif (kind == "filegroup"):
    return _merge_fields(ctx, ["srcs"], p)
  else:
    fail("Unknown rule kind")

transitive_attrs = ["tests", "srcs", "deps"]


def concat_files_impl(ctx, p):
  args = ctx.actions.args()
  file_parts = depset(transitive=[d[p].files for d in ctx.attr.deps])
  args.add_all(file_parts)
  ctx.actions.run_shell(
     outputs = [ctx.outputs.combined],
     inputs = file_parts,
     command = "cat >%s $@" % ctx.outputs.combined.path,
     arguments = [args]
  )
  return []

def concat_attrs(aspect):
  return {
    "deps": attr.label_list(aspects=[aspect]),
  }
