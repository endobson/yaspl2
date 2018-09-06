yaspl_provider = provider(fields = ["source_file", "module_name_file", "input_signatures",
                                    "signature", "transitive_objects"])
yaspl_src_provider = provider(fields = ["files"])

def _lib_impl(ctx):
  if (len(ctx.files.srcs) != 1):
    fail("Only one source is supported", "srcs")
  src_file = ctx.files.srcs[0]

  input_signatures = [dep[yaspl_provider].signature for dep in ctx.attr.deps]

  args = ctx.actions.args()
  args.add(ctx.outputs.object)
  args.add(ctx.outputs.signature)
  args.add(src_file)
  args.add_all(input_signatures)

  ctx.actions.run(
    inputs = input_signatures + ctx.files.srcs + [ctx.executable._library_compiler],
    outputs = [ctx.outputs.object, ctx.outputs.signature],
    mnemonic = "YasplCompile",
    executable = ctx.executable._library_compiler,
    arguments = [args],
  )

  ctx.actions.run_shell(
    inputs = ctx.files.srcs,
    outputs = [ctx.outputs.module_name],
    command = 'echo "$(head -n 1 %s | sed -e "s/#:module //")" > %s'
       % (src_file.path, ctx.outputs.module_name.path),
  )

  return [
    yaspl_provider(
      source_file = src_file,
      module_name_file = ctx.outputs.module_name,
      signature = ctx.outputs.signature,
      input_signatures = input_signatures,
      transitive_objects = depset(
        direct = [ctx.outputs.object],
        transitive = [dep[yaspl_provider].transitive_objects for dep in ctx.attr.deps]
      ),
    )
  ]

def _src_impl(ctx):
  transitive_srcs = depset(
    direct = ctx.files.srcs,
    transitive = [dep[yaspl_src_provider].files for dep in ctx.attr.deps],
  )
  args = ctx.actions.args()
  args.add(transitive_srcs)

  ctx.actions.run_shell(
    outputs = [ctx.outputs.file],
    command = 'printf "%%s\n" "$@" > %s' % ctx.outputs.file.path,
    arguments = [args],
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

  input_objects = depset(
    direct = [ctx.outputs.main_stub_object],
    transitive = [ctx.attr._runtime_objects.files] +
                 [dep[yaspl_provider].transitive_objects for dep in ctx.attr.deps],
  )

  args = ctx.actions.args()
  args.add(ctx.outputs.executable)
  args.add_all(input_objects)

  ctx.actions.run(
    inputs = depset(
      direct = [ctx.executable._linker],
      transitive = [input_objects],
    ),
    outputs = [ctx.outputs.executable],
    mnemonic = "YasplLink",
    executable = ctx.executable._linker,
    arguments = [args],
  )
  return []

_yaspl_src_file_type = FileType([".yaspl"])

_bootstrap_library_compiler = attr.label(
  default=Label("//prebuilt:library_compiler"),
  executable=True,
  allow_files=True,
  cfg="host",
)

_bootstrap_main_stub = attr.label(
  default=Label("//prebuilt:main_stub"),
  executable=True,
  allow_files=True,
  cfg="host",
)

_bootstrap_linker = attr.label(
  default=Label("//prebuilt:linker"),
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
    "signature": "%{name}.sig",
    "module_name": "%{name}.module_name",
  },
  attrs = {
    "srcs": attr.label_list(
      allow_files=_yaspl_src_file_type,
      mandatory=True,
      allow_empty=False
    ),
    "deps": attr.label_list(
      providers = [yaspl_provider],
    ),
    "_library_compiler": _bootstrap_library_compiler,
  }
)

def _yaspl_binary_rule(test):
  return rule(
    implementation = _bin_impl,
    outputs = {
      "main_stub_object": "%{name}_main.o",
    },
    executable = True,
    test = test,
    attrs = {
      "main_module": attr.string(mandatory=True),
      "deps": attr.label_list(
        providers = [yaspl_provider],
      ),
      "_main_stub": _bootstrap_main_stub,
      "_linker": _bootstrap_linker,
      "_runtime_objects": _yaspl_runtime_objects,
    },
  )
yaspl_binary = _yaspl_binary_rule(False)
yaspl_prim_test = _yaspl_binary_rule(True)

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
  },
)


def yaspl_test(name, main_module, srcs=[], deps=[], size="medium"):
  yaspl_library(
    # TODO make this testonly once aspects work better
    name = name + "_lib",
    srcs = srcs,
    deps = deps,
  )

  yaspl_prim_test(
    name = name,
    main_module = main_module,
    deps = [name + "_lib"],
    size = size,
    # TODO make this testonly once aspects work better
    testonly = 0,
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
