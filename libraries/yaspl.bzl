def _transitive_srcs(ctx):
  transitive_srcs = set(order="compile")
  for dep in ctx.attr.deps:
    transitive_srcs += dep.yaspl_transitive_srcs
  transitive_srcs += ctx.files.srcs
  return transitive_srcs

def _transitive_asms(ctx):
  transitive_asms = set(order="link")
  for dep in ctx.attr.deps:
    transitive_asms += dep.yaspl_transitive_asms
  return transitive_asms



def _lib_impl(ctx):

  transitive_srcs = _transitive_srcs(ctx)
  transitive_asms = _transitive_asms(ctx)
  transitive_src_paths = [src.path for src in transitive_srcs]

  ctx.action(
    inputs = list(transitive_srcs) + [ctx.executable._library_compiler],
    outputs = [ctx.outputs.asm],
    mnemonic = "YasplCompile",
    executable = ctx.executable._library_compiler,
    arguments = [ctx.outputs.asm.path] + list(transitive_src_paths)
  )

  return struct(
    yaspl_transitive_srcs = transitive_srcs,
    yaspl_transitive_asms = transitive_asms + [ctx.outputs.asm]
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

  ctx.action(
    inputs = [ctx.executable._main_stub],
    outputs = [ctx.outputs.main_stub_asm],
    mnemonic = "YasplGenerateMain",
    executable = ctx.executable._main_stub,
    arguments = [
      ctx.outputs.main_stub_asm.path,
      ctx.attr.main_module,
    ]
  )

  input_asms = list(_transitive_asms(ctx)) + [ctx.outputs.main_stub_asm]
  input_asm_paths = [asm.path for asm in input_asms]
  ctx.action(
    inputs = input_asms,
    outputs = [ctx.outputs.asm],
    mnemonic = "YasplCombineAssembly",
    command = "cat %s > %s" % (
      " ".join(input_asm_paths),
      ctx.outputs.asm.path
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

_yaspl_src_file_type = FileType([".yaspl"])
_asm_src_deps_attr = attr.label_list(
  providers = ["yaspl_transitive_asms", "yaspl_transitive_srcs"],
)

_asm_deps_attr = attr.label_list(
  providers = ["yaspl_transitive_asms"],
)

_src_deps_attr = attr.label_list(
  providers = ["yaspl_transitive_srcs"],
)

_bootstrap_library_compiler = attr.label(
 default=Label("//bootstrap:bootstrap_library_compiler"),
 executable=True,
 allow_files=True
)

_bootstrap_main_stub = attr.label(
 default=Label("//bootstrap:bootstrap_main_stub"),
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
    "deps": _asm_src_deps_attr,
    "_library_compiler": _bootstrap_library_compiler
  }
)

yaspl_binary = rule(
  implementation = _bin_impl,
  outputs = {
    "asm": "%{name}.s",
    "main_stub_asm": "%{name}_main.s",
    "object": "%{name}.o",
  },
  executable = True,
  attrs = {
    "main_module": attr.string(mandatory=True),
    "deps": _asm_deps_attr,
    "_main_stub": _bootstrap_main_stub,
  }
)


yaspl_prim_test = rule(
  implementation = _bin_impl,
  outputs = {
    "asm": "%{name}.s",
    "main_stub_asm": "%{name}_main.s",
    "object": "%{name}.o",
  },
  executable = True,
  test=True,
  attrs = {
    "main_module": attr.string(mandatory=True),
    "deps": _asm_deps_attr,
    "_main_stub": _bootstrap_main_stub,
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
    "deps": _src_deps_attr,
  }
)


def yaspl_test(name, main_module, srcs=[], deps=[]):
  yaspl_library(
    name = name + "_lib",
    srcs = srcs,
    deps = deps
  )

  yaspl_prim_test(
    name = name,
    main_module = main_module,
    deps = [name + "_lib"]
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
