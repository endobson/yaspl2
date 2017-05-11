def _bin_impl(ctx):
  # TODO check that srcs has exactly one value
  ctx.action(
    inputs = [ctx.executable._compiler] + ctx.files.srcs,
    outputs = [ctx.outputs.object],
    executable = ctx.executable._compiler,
    arguments = [
      ctx.files.srcs[0].path,
      ctx.outputs.object.path,
    ]
  )

  ctx.action(
    inputs = [ctx.outputs.object],
    outputs = [ctx.outputs.executable],
    executable = ctx.executable._linker,
    arguments = [
      ctx.outputs.executable.path,
      ctx.outputs.object.path,
    ]
  )

  return struct()

prim_binary = rule(
  implementation = _bin_impl,
  outputs = {
    "object": "%{name}.o",
  },
  executable = True,
  attrs = {
    "srcs": attr.label_list(
      allow_files=FileType([".prim"]),
      mandatory=True,
      non_empty=True
    ),
    "_compiler": attr.label(
      default=Label("//libraries/prim-language:prim-language-compiler"),
      executable=True,
      cfg="host",
    ),
    "_linker": attr.label(
      default=Label("//libraries:linker"),
      executable=True,
      cfg="host",
    ),
  }
)
