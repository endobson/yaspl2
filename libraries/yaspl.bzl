def _lib_impl(ctx):
  ctx.action(
    outputs = [ctx.outputs.asm],
    command = 
      "%s unknown < %s > %s" % (
        ctx.executable._compiler.path,
        ctx.files.srcs[0].path,
        ctx.outputs.asm.path
      )
  )


yaspl_src_file_type = FileType([".yaspl"])

yaspl_library = rule(
  implementation = _lib_impl,
  outputs = {
    "asm": "%{name}.s"
  },
  attrs = {
    "srcs": attr.label_list(
      allow_files=yaspl_src_file_type,
      mandatory=True,
      non_empty=True
    ),
    "_compiler": attr.label(
      default=Label("//:compiler"),
      executable=True,
      allow_files=True
    ),
  }
)
