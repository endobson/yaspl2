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

def _lib_impl(ctx):
  # TODO check that srcs has exactly one value
  ctx.actions.run(
    inputs = [ctx.executable._compiler] + ctx.files.srcs,
    outputs = [ctx.outputs.object],
    executable = ctx.executable._compiler,
    arguments = [
      ctx.files.srcs[0].path,
      ctx.outputs.object.path,
    ]
  )

  return struct()

prim_library = rule(
  implementation = _lib_impl,
  outputs = {
    "object": "%{name}.o",
  },
  attrs = {
    "srcs": attr.label_list(
      allow_files=FileType([".prim"]),
      mandatory=True,
      non_empty=True
    ),
    "_compiler": attr.label(
      default=Label("//prebuilt:prim_language_library_compiler"),
      executable=True,
      allow_files=True,
      cfg="host",
    ),
    "_linker": attr.label(
      default=Label("//prebuilt:linker"),
      executable=True,
      allow_files=True,
      cfg="host",
    ),
  }
)


def _binary_test_impl(ctx):
  ctx.file_action(
    output = ctx.outputs.executable,
    content =
      "#!/bin/sh\n" +
      "STDOUT=$(" + ctx.executable.binary.short_path + " " + " ".join(ctx.attr.binary_args) + ")\n"+
      "EXIT_CODE=$?\n" +
      "PASSED=0\n" +
      'if [ "$EXIT_CODE" -ne "' + str(ctx.attr.exit_code) + '" ]\n' +
      "then\n" +
      '  echo "Expected exit code: "' + str(ctx.attr.exit_code) + '\n' +
      '  echo "Got exit code: $EXIT_CODE"\n' +
      "  PASSED=1\n" +
      "fi\n" +
      'if [ "$STDOUT" != "' + ctx.attr.output + '" ]\n' +
      "then\n" +
      '  echo "Expected output: \\""' + "" + '\\"\n' +
      '  echo "Got output: \\"${STDOUT}\\""\n' +
      "  PASSED=1\n" +
      "fi\n" +

      "exit $PASSED\n",
    executable = True)

  return struct(
    runfiles=ctx.runfiles(files=[ctx.executable.binary]),
  )


binary_test = rule(
  implementation = _binary_test_impl,
  outputs = {},
  test = True,
  attrs = {
    "binary": attr.label(
       allow_files = True,
       executable = True,
       cfg = "data",
    ),
    "binary_args": attr.string_list(),
    "exit_code": attr.int(),
    "output": attr.string(default = "")
  }
)
