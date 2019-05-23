load("//libraries:yaspl.bzl", "yaspl_provider")
load(":yaspl-transitive-rules.bzl", "transitivify_impl", "transitive_attrs",
     "concat_files_impl")

yaspl_validate_ir_provider = provider(fields=["files"])

def _yaspl_library_validate_ir_impl(target, ctx):
  output_file = ctx.actions.declare_file(ctx.label.name + ".validate_ir_part")
  src_file = target[yaspl_provider].source_file
  input_signatures = target[yaspl_provider].input_signatures

  args = ctx.actions.args()
  args.add(output_file)
  args.add(src_file)
  args.add_all(input_signatures)

  ctx.actions.run(
    inputs = [src_file] + input_signatures,
    outputs = [output_file],
    executable = ctx.executable._ir_validator,
    arguments = [args]
  )

  return yaspl_validate_ir_provider(files=depset([output_file]))

def _yaspl_validate_ir_impl(target, ctx):
  return [transitivify_impl(target, ctx, _yaspl_library_validate_ir_impl,
                            yaspl_validate_ir_provider)]


yaspl_validate_ir = aspect(
  implementation = _yaspl_validate_ir_impl,
  attr_aspects = transitive_attrs,
  provides = [yaspl_validate_ir_provider],
  attrs = {
    "_ir_validator": attr.label(
       default=Label("//tools:aspect-ir-validator"),
       executable=True,
       cfg="host",
    )
  }
)

def _yaspl_validate_ir_rule_impl(ctx):
  return concat_files_impl(ctx, yaspl_validate_ir_provider)

yaspl_validate_ir_rule = rule(
  implementation = _yaspl_validate_ir_rule_impl,
  attrs = {
    "deps": attr.label_list(aspects=[yaspl_validate_ir]),
  },
  outputs = {
    "combined": "%{name}.validate_ir"
  },
)
