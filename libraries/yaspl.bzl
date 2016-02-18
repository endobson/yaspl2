def _lib_impl(ctx):

  transitive_srcs = set(order="compile")
  for dep in ctx.attr.deps:
    transitive_srcs += dep.yaspl_transitive_srcs
  transitive_srcs += ctx.files.srcs

  transitive_src_paths = [src.path for src in transitive_srcs]

  ctx.action(
    outputs = [ctx.outputs.asm],
    mnemonic = "YasplCompile",
    executable = ctx.executable._compiler,
    arguments = [
      ctx.outputs.asm.path,
      "unknown",
    ] + list(transitive_src_paths)
  )


  return struct(
    yaspl_transitive_srcs = transitive_srcs
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
    "deps": attr.label_list(
      providers = ["yaspl_transitive_srcs"],
    ),
    "_compiler": attr.label(
      default=Label("//:compiler"),
      executable=True,
      allow_files=True
    ),
  }
)
