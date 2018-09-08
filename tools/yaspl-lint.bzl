load("//libraries:yaspl.bzl", "yaspl_provider")

yaspl_lint_provider = provider(fields=["files"])

def _yaspl_library_lint_impl(target, ctx):
  output = ctx.new_file(ctx.label.name + ".lint")
  src_file = target[yaspl_provider].source_file
  input_signatures = target[yaspl_provider].input_signatures

  args = ctx.actions.args()
  args.add(src_file)
  args.add(output)
  args.add_all(input_signatures)

  ctx.actions.run(
    inputs = [src_file] + input_signatures,
    outputs = [output],
    executable = ctx.executable._linter,
    arguments = [args],
  )

  local_provider = yaspl_lint_provider(files=depset([output]))
  return [_merge_providers([local_provider] + _extract_providers(ctx.rule.attr.deps))]

def _yaspl_binary_lint_impl(target, ctx):
  return [_merge_providers(_extract_providers(ctx.rule.attr.deps))]

def _test_suite_lint_impl(target, ctx):
  return [_merge_providers(_extract_providers(ctx.rule.attr.tests))]

def _filegroup_lint_impl(target, ctx):
  return [_merge_providers(_extract_providers(ctx.rule.attr.srcs))]

def _merge_providers(providers):
  files = depset(transitive=[p.files for p in providers])
  return yaspl_lint_provider(files = files)
def _extract_providers(objs):
  return [obj[yaspl_lint_provider] for obj in objs]

def _yaspl_lint_impl(target, ctx):
  kind = ctx.rule.kind
  if (kind == "yaspl_library"):
    return _yaspl_library_lint_impl(target, ctx)
  elif (kind == "yaspl_binary"):
    return _yaspl_binary_lint_impl(target, ctx)
  elif (kind == "test_suite"):
    return _test_suite_lint_impl(target, ctx)
  elif (kind == "filegroup"):
    return _filegroup_lint_impl(target, ctx)
  else:
    fail("Unknown rule kind")

yaspl_lint = aspect(
  implementation = _yaspl_lint_impl,
  attr_aspects = ["tests", "srcs", "deps"],
  attrs = {
    "_linter": attr.label(
       default=Label("//tools:aspect-linter"),
       executable=True,
       cfg="host",
    )
  }
)

def _yaspl_lint_rule_impl(ctx):
  args = ctx.actions.args()
  lint_file_parts = depset(transitive=[d[yaspl_lint_provider].files for d in ctx.attr.deps])
  args.add_all(lint_file_parts)
  ctx.actions.run_shell(
     outputs = [ctx.outputs.lint],
     inputs = lint_file_parts,
     command = "cat  >%s $@" % ctx.outputs.lint.path,
     arguments = [args]
  )
  return []

yaspl_lint_rule = rule(
  implementation = _yaspl_lint_rule_impl,
  attrs = {
    "deps": attr.label_list(aspects=[yaspl_lint])
  },
  outputs = {
    "lint": "%{name}.lint"
  },
)
