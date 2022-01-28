# TODO(endobson) Avoid duplication of this with yaspl_src rule.

def _filegroup_list_impl(ctx):
  out_file = ctx.actions.declare_file(ctx.label.name)

  args = ctx.actions.args()
  args.add_all(ctx.attr.dep[DefaultInfo].files)

  ctx.actions.run_shell(
    outputs = [out_file],
    arguments = [args],
    command = 'printf "%%s\n" "$@" > %s'
      % (out_file.path),
  )

  return [DefaultInfo(files=depset([out_file]))]


filegroup_list = rule(
  implementation = _filegroup_list_impl,
  attrs = {
    "dep": attr.label(
      mandatory=True,
    ),
  }
)
