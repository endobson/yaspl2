def _transitive_srcs(ctx):
  transitive_srcs = set(order="compile")
  for dep in ctx.attr.deps:
    transitive_srcs += dep.yaspl_transitive_srcs
  transitive_srcs += ctx.files.srcs
  return transitive_srcs

def _transitive_objects(ctx):
  transitive_objects = set(order="link")
  for dep in ctx.attr.deps:
    transitive_objects += dep.yaspl_transitive_objects
  return transitive_objects



def _lib_impl(ctx):

  transitive_srcs = _transitive_srcs(ctx)
  transitive_objects = _transitive_objects(ctx)
  transitive_src_paths = [src.path for src in transitive_srcs]

  ctx.action(
    inputs = list(transitive_srcs) + [ctx.executable._library_compiler],
    outputs = [ctx.outputs.object],
    mnemonic = "YasplCompile",
    executable = ctx.executable._library_compiler,
    arguments = [ctx.outputs.object.path] + list(transitive_src_paths)
  )


  return struct(
    yaspl_transitive_srcs = transitive_srcs,
    yaspl_transitive_objects = transitive_objects + [ctx.outputs.object]
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
    outputs = [ctx.outputs.main_stub_object],
    mnemonic = "YasplGenerateMain",
    executable = ctx.executable._main_stub,
    arguments = [
      ctx.outputs.main_stub_object.path,
      ctx.attr.main_module,
    ]
  )

  input_objects = list(_transitive_objects(ctx)) + [ctx.outputs.main_stub_object]
  input_object_paths = [obj.path for obj in input_objects]
  ctx.action(
    inputs = input_objects,
    outputs = [ctx.outputs.executable],
    mnemonic = "YasplLink",
    command = "ld -arch x86_64 " +
    "-macosx_version_min 10.11 " +
    "-static " +
    "-no_uuid " +
    "-sectcreate __DATA __data /dev/null " +
    "%s -o %s" % (
      " ".join(input_object_paths),
      ctx.outputs.executable.path
    )
  )

_yaspl_src_file_type = FileType([".yaspl"])

_src_deps_attr = attr.label_list(
  providers = ["yaspl_transitive_srcs"],
)

_bootstrap_library_compiler = attr.label(
 default=Label("//bootstrap:bootstrap_library_compiler"),
 executable=True,
 allow_files=True,
 cfg="host",
)

_bootstrap_main_stub = attr.label(
 default=Label("//bootstrap:bootstrap_main_stub"),
 executable=True,
 allow_files=True,
 cfg="host",
)

yaspl_library = rule(
  implementation = _lib_impl,
  outputs = {
    "object": "%{name}.o"
  },
  attrs = {
    "srcs": attr.label_list(
      allow_files=_yaspl_src_file_type,
      mandatory=True,
      non_empty=True
    ),
    "deps": _src_deps_attr,
    "_library_compiler": _bootstrap_library_compiler
  }
)

yaspl_binary = rule(
  implementation = _bin_impl,
  outputs = {
    "main_stub_object": "%{name}_main.o",
  },
  executable = True,
  attrs = {
    "main_module": attr.string(mandatory=True),
    "deps": attr.label_list(),
    "_main_stub": _bootstrap_main_stub,
  }
)


yaspl_prim_test = rule(
  implementation = _bin_impl,
  outputs = {
    "main_stub_object": "%{name}_main.o",
  },
  executable = True,
  test=True,
  attrs = {
    "main_module": attr.string(mandatory=True),
    "deps": attr.label_list(),
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
