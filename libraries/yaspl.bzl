def _transitive_srcs(ctx):
  transitive_srcs = set(order="compile")
  for dep in ctx.attr.deps:
    transitive_srcs += dep.yaspl_transitive_srcs
  transitive_srcs += ctx.files.srcs
  return transitive_srcs


def _lib_impl(ctx):

  transitive_srcs = _transitive_srcs(ctx)
  transitive_src_paths = [src.path for src in transitive_srcs]

  ctx.action(
    outputs = [ctx.outputs.asm],
    mnemonic = "YasplCompile",
    executable = ctx.executable._library_compiler,
    arguments = [ctx.outputs.asm.path] + list(transitive_src_paths)
  )

  return struct(
    yaspl_transitive_srcs = transitive_srcs
  )

def _src_impl(ctx):
  transitive_srcs = _transitive_srcs(ctx)
  transitive_src_paths = [src.path for src in transitive_srcs]

  ctx.file_action(
    output = ctx.outputs.file,
    content = "\n".join(transitive_src_paths)
  )

  return struct(
    yaspl_transitive_srcs = transitive_srcs
  )


def _bin_impl(ctx):

  transitive_srcs = _transitive_srcs(ctx)
  transitive_src_paths = [src.path for src in transitive_srcs]

  ctx.action(
    inputs = list(transitive_srcs),
    outputs = [ctx.outputs.asm],
    mnemonic = "YasplCompile",
    executable = ctx.executable._compiler,
    arguments = [
      ctx.outputs.asm.path,
      ctx.attr.main_module,
    ] + transitive_src_paths
  )


  ctx.action(
    inputs = [ctx.outputs.object],
    outputs = [ctx.outputs.executable],
    mnemonic = "YasplLink",
    command = "ld -arch x86_64 " +
    "-macosx_version_min 10.11 " +
    "-static " +
    "-no_uuid " +
    "-sectcreate __DATA __data /dev/null " +
    "%s -o %s" % (
      ctx.outputs.object.path,
      ctx.outputs.executable.path
    )
  )


  ctx.action(
    inputs = [ctx.outputs.asm],
    outputs = [ctx.outputs.object],
    mnemonic = "YasplAssemble",
    command = "as %s -o %s" % (
      ctx.outputs.asm.path,
      ctx.outputs.object.path
    )
  )


  return struct(
    yaspl_transitive_srcs = transitive_srcs
  )

def _test_impl(ctx):

  transitive_srcs = _transitive_srcs(ctx)
  transitive_src_paths = [src.path for src in transitive_srcs]

  ctx.action(
    inputs = list(transitive_srcs),
    outputs = [ctx.outputs.asm],
    mnemonic = "YasplCompile",
    executable = ctx.executable._compiler,
    arguments = [
      ctx.outputs.asm.path,
      ctx.attr.main_module,
    ] + transitive_src_paths
  )


  ctx.action(
    inputs = [ctx.outputs.object],
    outputs = [ctx.outputs.executable],
    mnemonic = "YasplLink",
    command = "ld -arch x86_64 " +
    "-macosx_version_min 10.11 " +
    "-static " +
    "-no_uuid " +
    "-sectcreate __DATA __data /dev/null " +
    "%s -o %s" % (
      ctx.outputs.object.path,
      ctx.outputs.executable.path
    )
  )

  ctx.action(
    inputs = [ctx.outputs.asm],
    outputs = [ctx.outputs.object],
    mnemonic = "YasplAssemble",
    command = "as %s -o %s" % (
      ctx.outputs.asm.path,
      ctx.outputs.object.path
    )
  )


  return struct(
    yaspl_transitive_srcs = transitive_srcs
  )


_yaspl_src_file_type = FileType([".yaspl"])
_deps_attr = attr.label_list(
  providers = ["yaspl_transitive_srcs"],
)

_bootstrap_compiler = attr.label(
 default=Label("//bootstrap:bootstrap_compiler"),
 executable=True,
 allow_files=True
)

_bootstrap_library_compiler = attr.label(
 default=Label("//bootstrap:bootstrap_library_compiler"),
 executable=True,
 allow_files=True
)

yaspl_library = rule(
  implementation = _lib_impl,
  outputs = {
    "asm": "%{name}.s"
  },
  attrs = {
    "srcs": attr.label_list(
      allow_files=_yaspl_src_file_type,
      mandatory=True,
      non_empty=True
    ),
    "deps": _deps_attr,
    "_compiler": _bootstrap_compiler,
    "_library_compiler": _bootstrap_library_compiler
  }
)

yaspl_binary = rule(
  implementation = _bin_impl,
  outputs = {
    "asm": "%{name}.s",
    "object": "%{name}.o",
  },
  executable = True,
  attrs = {
    "main_module": attr.string(mandatory=True),
    "srcs": attr.label_list(),
    "deps": _deps_attr,
    "_compiler": _bootstrap_compiler,
    "_library_compiler": _bootstrap_library_compiler
  }
)

yaspl_test = rule(
  implementation = _test_impl,
  outputs = {
    "asm": "%{name}.s",
    "object": "%{name}.o",
  },
  executable = True,
  test=True,
  attrs = {
    "main_module": attr.string(mandatory=True),
    "srcs": attr.label_list(
      allow_files=_yaspl_src_file_type,
      mandatory=True,
      non_empty=True
    ),
    "deps": _deps_attr,
    "_compiler": _bootstrap_compiler
  }
)

yaspl_srcs = rule(
  implementation = _src_impl,
  outputs = {
    "file": "%{name}.list",
  },
  attrs = {
    "srcs": attr.label_list(
      allow_files=_yaspl_src_file_type,
    ),
    "deps": _deps_attr,
  }
)


def yaspl_bootstrap_library(name, srcs, deps=[]):
  yaspl_library(
    name=name,
    srcs=srcs,
    deps=deps,
  )

  yaspl_srcs(
    name=name + ".src",
    srcs=srcs,
    deps=[dep + ".src" for dep in deps],
  )

  source_file_suffix = ".src_dep"
  source_group_suffix = ".srcs"

  for src in srcs:
    native.filegroup(
      name=src + source_file_suffix,
      srcs=[src],
      data=[dep + source_group_suffix for dep in deps],
    )

  native.filegroup(
    name=name + source_group_suffix,
    data=[src + source_file_suffix for src in srcs],
  )
