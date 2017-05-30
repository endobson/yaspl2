def _transitive_srcs(ctx):
  transitive_srcs = _dependent_srcs(ctx)
  transitive_srcs += ctx.files.srcs
  return transitive_srcs

def _dependent_srcs(ctx):
  dependent_srcs = set(order="compile")
  for dep in ctx.attr.deps:
    dependent_srcs += dep.yaspl_transitive_srcs
  return dependent_srcs

def _transitive_objects(ctx):
  transitive_objects = set(order="compile")
  for dep in ctx.attr.deps:
    transitive_objects += dep.yaspl_transitive_objects
  return transitive_objects

def _dependent_dep_infos(ctx):
  dependent_dep_infos = set(order="compile")
  for dep in ctx.attr.deps:
    dependent_dep_infos += dep.yaspl_transitive_deps
  return dependent_dep_infos



def _lib_impl(ctx):
  if (len(ctx.files.srcs) != 1):
    fail("Only one source is supported", "srcs")
  src_path = ctx.files.srcs[0].path

  direct_signatures = [dep.yaspl_signature for dep in ctx.attr.deps]
  direct_signature_paths = [sig.path for sig in direct_signatures]

  ctx.action(
    inputs = direct_signatures + ctx.files.srcs + [ctx.executable._library_compiler],
    outputs = [ctx.outputs.object, ctx.outputs.signature],
    mnemonic = "YasplCompile",
    executable = ctx.executable._library_compiler,
    arguments = [ctx.outputs.object.path, ctx.outputs.signature.path, src_path] + direct_signature_paths
  )

  dep_info = struct(
    source = ctx.files.srcs[0],
    object = ctx.outputs.object,
    signature = ctx.outputs.signature
  )

  dependent_dep_infos = _dependent_dep_infos(ctx)
  dependent_objects = [dep.object for dep in dependent_dep_infos]

  return struct(
    yaspl_transitive_objects = dependent_objects + [ctx.outputs.object],
    yaspl_signature = ctx.outputs.signature,
    yaspl_transitive_deps = list(dependent_dep_infos + [dep_info]),
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
    inputs = [ctx.executable._linker] + input_objects,
    outputs = [ctx.outputs.executable],
    mnemonic = "YasplLink2",
    executable = ctx.executable._linker,
    arguments = [ctx.outputs.executable.path] + input_object_paths
  )

_yaspl_src_file_type = FileType([".yaspl"])


_bootstrap_library_compiler = attr.label(
 default=Label("//bootstrap:library_compiler"),
 executable=True,
 allow_files=True,
 cfg="host",
)

_bootstrap_main_stub = attr.label(
 default=Label("//bootstrap:main_stub"),
 executable=True,
 allow_files=True,
 cfg="host",
)

_bootstrap_linker = attr.label(
 default=Label("//bootstrap:linker"),
 executable=True,
 allow_files=True,
 cfg="host",
)


yaspl_library = rule(
  implementation = _lib_impl,
  outputs = {
    "object": "%{name}.o",
    "signature": "%{name}.sig"
  },
  attrs = {
    "srcs": attr.label_list(
      allow_files=_yaspl_src_file_type,
      mandatory=True,
      non_empty=True
    ),
    "deps": attr.label_list(
       providers = ["yaspl_transitive_deps"],
    ),
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
    "_linker": _bootstrap_linker,
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
    "_linker": _bootstrap_linker,
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
    "deps": attr.label_list(
      providers = ["yaspl_transitive_srcs"],
    )
  }
)


def yaspl_test(name, main_module, srcs=[], deps=[], size="medium"):
  yaspl_library(
    testonly = 1,
    name = name + "_lib",
    srcs = srcs,
    deps = deps
  )

  yaspl_prim_test(
    name = name,
    main_module = main_module,
    deps = [name + "_lib"],
    size = size
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
