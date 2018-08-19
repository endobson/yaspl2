yaspl_provider = provider(fields = ["source_file", "input_signatures", "signature", "transitive_objects"])
yaspl_src_provider = provider(fields = ["files"])

def _dependent_srcs(ctx):
  dependent_srcs = depset(order="postorder")
  for dep in ctx.attr.deps:
    dependent_srcs += dep[yaspl_src_provider].files
  return dependent_srcs

def _dependent_objects(ctx):
  dependent_objects = depset(order="postorder")
  for dep in ctx.attr.deps:
    dependent_objects += dep[yaspl_provider].transitive_objects
  return dependent_objects


def _lib_impl(ctx):
  if (len(ctx.files.srcs) != 1):
    fail("Only one source is supported", "srcs")
  src_path = ctx.files.srcs[0].path

  input_signatures = [dep[yaspl_provider].signature for dep in ctx.attr.deps]
  input_signature_paths = [sig.path for sig in input_signatures]

  ctx.action(
    inputs = input_signatures + ctx.files.srcs + [ctx.executable._library_compiler],
    outputs = [ctx.outputs.object, ctx.outputs.signature],
    mnemonic = "YasplCompile",
    executable = ctx.executable._library_compiler,
    arguments = [ctx.outputs.object.path, ctx.outputs.signature.path, src_path] + input_signature_paths
  )

  return [
    yaspl_provider(
      source_file = ctx.files.srcs[0],
      input_signatures = input_signatures,
      signature = ctx.outputs.signature,
      transitive_objects = _dependent_objects(ctx) + [ctx.outputs.object]
    )
  ]

def _src_impl(ctx):
  transitive_srcs = _dependent_srcs(ctx) + ctx.files.srcs
  transitive_src_paths = [src.path for src in transitive_srcs]

  ctx.file_action(
    output = ctx.outputs.file,
    content = "\n".join(transitive_src_paths)
  )

  return [
    yaspl_src_provider(files = transitive_srcs)
  ]

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

  input_objects = list(_dependent_objects(ctx) + [ctx.outputs.main_stub_object] +
                                               ctx.attr._runtime_objects.files)
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

_yaspl_runtime_objects = attr.label(
 default=Label("//libraries/yaspl/runtime:runtime"),
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
       providers = [yaspl_provider],
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
    "_runtime_objects": _yaspl_runtime_objects,
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
    "_runtime_objects": _yaspl_runtime_objects,
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
      providers = [yaspl_src_provider],
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


def _relative_label(label_string, suffix):
  if label_string.startswith("//"):
    label = Label(label_string)
    return "//" + label.package + ":" + label.name + suffix
  elif label_string.startswith(":"):
    return label_string + suffix
  else:
    fail("Label is not valid: " + label_string)

def yaspl_bootstrap_library(name, srcs, deps=[]):
  yaspl_library(
    name=name,
    srcs=srcs,
    deps=deps,
  )

  yaspl_srcs(
    name=name + ".src",
    srcs=srcs,
    deps=[_relative_label(dep, ".src") for dep in deps],
  )

  source_file_suffix = ".src_dep"
  source_group_suffix = ".srcs"

  for src in srcs:
    native.filegroup(
      name=src + source_file_suffix,
      srcs=[src],
      data=[_relative_label(dep, source_group_suffix) for dep in deps],
    )

  native.filegroup(
    name=name + source_group_suffix,
    data=[src + source_file_suffix for src in srcs],
  )
