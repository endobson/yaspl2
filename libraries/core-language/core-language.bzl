def _lib_impl(ctx):
  if (len(ctx.files.srcs) != 1):
    fail("Must have exactly one source file.", "srcs")

  output_object = ctx.actions.declare_file("%s.o" % ctx.attr.name)
  output_signature = ctx.actions.declare_file("%s.sig" % ctx.attr.name)

  toolchain = ctx.toolchains["//libraries/core-language:core_language_toolchain"]

  ctx.actions.run(
    inputs = ctx.files.srcs,
    outputs = [output_object, output_signature],
    executable = ctx.executable._compiler,
    arguments = [
      toolchain.platform,
      output_object.path,
      output_signature.path,
      ctx.files.srcs[0].path,
    ]
  )

  return [
    DefaultInfo(
      files = depset([output_object, output_signature])
    ),
  ]

core_library = rule(
  implementation = _lib_impl,
  toolchains = ["//libraries/core-language:core_language_toolchain"],
  attrs = {
    "srcs": attr.label_list(
      allow_files=[".core"],
      mandatory=True,
      allow_empty=True
    ),
    "_compiler": attr.label(
      default=Label("//libraries/core-language:compiler"),
      executable=True,
      allow_files=True,
      cfg="host",
    ),
  }
)


def _core_language_toolchain_impl(ctx):
  return [
    platform_common.ToolchainInfo(
      platform = ctx.attr.platform,
    ),
  ]

core_language_toolchain = rule(
  implementation = _core_language_toolchain_impl,
  attrs = {
    'platform': attr.string(),
  }
)
