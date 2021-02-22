load("//libraries:yaspl.bzl", "yaspl_provider")

def _lib_impl(ctx):
  if (len(ctx.files.srcs) != 1):
    fail("Must have exactly one source file.", "srcs")
  src_file = ctx.files.srcs[0]

  output_object = ctx.actions.declare_file("%s.o" % ctx.attr.name)
  output_signature = ctx.actions.declare_file("%s.sig" % ctx.attr.name)
  output_module_name = ctx.actions.declare_file("%s.module_name" % ctx.attr.name)

  toolchain = ctx.toolchains["//libraries/core-language:core_language_toolchain"]

  args = ctx.actions.args()
  args.add(toolchain.platform)
  args.add(output_object.path)
  args.add(output_signature.path)
  args.add(src_file.path)
  args.add_all(ctx.files.signatures)

  ctx.actions.run(
    inputs = ctx.files.srcs + ctx.files.signatures,
    outputs = [output_object, output_signature],
    executable = ctx.executable._compiler,
    arguments = [args],
  )

  ctx.actions.run_shell(
    inputs = ctx.files.srcs,
    outputs = [output_module_name],
    command = 'echo "$(head -n 1 %s | sed -e "s/#:module //")" > %s'
       % (src_file.path, output_module_name.path),
  )


  return [
    DefaultInfo(
      files = depset([output_object, output_signature])
    ),
    yaspl_provider(
      signature = output_signature,
      transitive_objects = depset([output_object]),
      module_name_file = output_module_name
    )
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
    "signatures": attr.label_list(
      allow_files=[".sig"],
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
