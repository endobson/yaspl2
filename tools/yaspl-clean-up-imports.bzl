load("//libraries:yaspl.bzl", "yaspl_provider")
load(":yaspl-transitive-rules.bzl", "transitivify_impl", "transitive_attrs",
     "concat_files_impl", "concat_attrs")

yaspl_clean_up_imports_provider = provider(fields=["files"])

def _yaspl_library_clean_up_imports_impl(target, ctx):
  target_yaspl_provider = target[yaspl_provider]
  input_signatures = target_yaspl_provider.input_signatures
  source_file = target_yaspl_provider.source_file

  if not source_file.is_source:
    return yaspl_clean_up_imports_provider(files=depset())

  output_file = ctx.actions.declare_file(ctx.label.name + ".cleaned_up_imports")
  args = ctx.actions.args()
  args.add_all(input_signatures)

  ctx.actions.run_shell(
     outputs = [output_file],
     inputs = [source_file] + input_signatures,
     tools = [ctx.executable._clean_up_imports],
     command = '%s %s $@ >%s' %
         (ctx.executable._clean_up_imports.path, source_file.path, output_file.path),
     arguments = [args],
  )

  command_file = ctx.actions.declare_file(ctx.label.name + ".clean_up_patch")
  ctx.actions.run_shell(
     outputs = [command_file],
     inputs = [output_file, target_yaspl_provider.source_file],
     command = ("diff -u --label %s %s --label %s %s > %s; " +
                "exit_code=$?; [ $exit_code -eq 0 ] || [ $exit_code -eq 1 ]") %
         (source_file.path, source_file.path, source_file.path, output_file.path, command_file.path)
  )

  return yaspl_clean_up_imports_provider(files=depset([command_file]))

def _yaspl_clean_up_imports_impl(target, ctx):
  return [transitivify_impl(target, ctx, _yaspl_library_clean_up_imports_impl, yaspl_clean_up_imports_provider)]

yaspl_clean_up_imports = aspect(
  implementation = _yaspl_clean_up_imports_impl,
  attr_aspects = transitive_attrs,
  provides = [yaspl_clean_up_imports_provider],
  attrs = {
    "_clean_up_imports": attr.label(
       default=Label("//tools:aspect-clean-up-imports"),
       executable=True,
       allow_files=True,
       cfg="host",
    ),
  }
)

def _yaspl_clean_up_imports_rule_impl(ctx):
  return concat_files_impl(ctx, yaspl_clean_up_imports_provider)

yaspl_clean_up_imports_rule = rule(
  implementation = _yaspl_clean_up_imports_rule_impl,
  attrs = concat_attrs(yaspl_clean_up_imports),
  outputs = {
    "combined": "%{name}.clean_up_imports"
  },
)
