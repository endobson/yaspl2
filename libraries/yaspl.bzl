yaspl_provider = provider(fields = ["source_file", "module_name_file", "input_signatures",
                                    "signature", "transitive_objects"])
yaspl_src_provider = provider(fields = ["files"])

def _lib_impl(ctx):
  if (len(ctx.files.srcs) != 1):
    fail("Only one source is supported", "srcs")
  src_file = ctx.files.srcs[0]

  toolchain = ctx.toolchains["//libraries/yaspl:yaspl_toolchain"]
  input_signatures = [dep[yaspl_provider].signature for dep in ctx.attr.deps]

  args = ctx.actions.args()
  args.add(toolchain.platform)
  args.add(ctx.outputs.object)
  args.add(ctx.outputs.signature)
  args.add(src_file)
  args.add_all(input_signatures)

  ctx.actions.run(
    inputs = input_signatures + ctx.files.srcs,
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
    ),
    DefaultInfo(files=depset([ctx.outputs.object])),
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
    yaspl_src_provider(files = transitive_srcs),
  ]

def _bin_impl(ctx):
  if (len(ctx.attr.deps) != 1):
    fail("Only one dep is supported", "deps")
  dep = ctx.attr.deps[0]

  toolchain = ctx.toolchains["//libraries/yaspl:yaspl_toolchain"]

  ctx.actions.run_shell(
    inputs = [dep[yaspl_provider].module_name_file],
    tools = [ctx.executable._main_stub],
    outputs = [ctx.outputs.main_stub_object],
    mnemonic = "YasplGenerateMain",
    command = '%s %s %s "$(cat %s)"' % (
      ctx.executable._main_stub.path,
      toolchain.platform,
      ctx.outputs.main_stub_object.path,
      dep[yaspl_provider].module_name_file.path,
    )
  )

  input_objects = depset(
    direct = [ctx.outputs.main_stub_object],
    transitive = [ctx.attr._runtime_objects.files,
                  dep[yaspl_provider].transitive_objects],
  )

  args = ctx.actions.args()
  args.add(toolchain.platform)
  args.add(ctx.outputs.executable)
  args.add_all(input_objects)

  ctx.actions.run(
    inputs = input_objects,
    tools = [ctx.executable._linker],
    outputs = [ctx.outputs.executable],
    mnemonic = "YasplLink",
    executable = ctx.executable._linker,
    arguments = [args],
  )
  return [
    DefaultInfo(files=depset([ctx.outputs.executable])),
  ]

_yaspl_src_file_extensions = [".yaspl"]

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
  toolchains = ["//libraries/yaspl:yaspl_toolchain"],
  attrs = {
    "srcs": attr.label_list(
      allow_files=_yaspl_src_file_extensions,
      mandatory=True,
      allow_empty=False,
    ),
    "deps": attr.label_list(
      providers = [yaspl_provider],
    ),
    "source_rule_name": attr.string(),
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
    toolchains = ["//libraries/yaspl:yaspl_toolchain"],
    attrs = {
      "deps": attr.label_list(
        providers = [yaspl_provider],
      ),
      "_main_stub": _bootstrap_main_stub,
      "_linker": _bootstrap_linker,
      "_runtime_objects": _yaspl_runtime_objects,
    },
  )
yaspl_prim_binary = _yaspl_binary_rule(False)
yaspl_prim_test = _yaspl_binary_rule(True)

yaspl_srcs = rule(
  implementation = _src_impl,
  outputs = {
    "file": "%{name}.list",
  },
  attrs = {
    "srcs": attr.label_list(
      allow_files=_yaspl_src_file_extensions,
    ),
    "deps": attr.label_list(
      providers = [yaspl_src_provider],
    )
  },
)

def _yaspl_toolchain_impl(ctx):
  return [
    platform_common.ToolchainInfo(
      platform = ctx.attr.platform,
    ),
  ]

yaspl_toolchain = rule(
  implementation = _yaspl_toolchain_impl,
  attrs = {
    'platform': attr.string(),
  }
)

def yaspl_test(name, srcs=[], deps=[], size="medium"):
  yaspl_library(
    # TODO make this testonly once aspects work better
    name = name + "_lib",
    source_rule_name = name,
    srcs = srcs,
    deps = deps,
  )

  yaspl_prim_test(
    name = name,
    deps = [name + "_lib"],
    size = size,
    # TODO make this testonly once aspects work better
    testonly = 0,
  )

def yaspl_binary(name, srcs=[], deps=[]):
  # TODO claim lib suffix here
  yaspl_library(
    name = name + "_mainlib",
    source_rule_name = name,
    srcs = srcs,
    deps = deps,
  )

  yaspl_prim_binary(
    name = name,
    deps = [name + "_mainlib"],
  )

def _relative_label(label_string, suffix):
  if label_string.startswith("//"):
    label = Label(label_string)
    return "//" + label.package + ":" + label.name + suffix
  elif label_string.startswith(":"):
    return label_string + suffix
  else:
    fail("Label is not valid: " + label_string)

def _bootstrap_inner(name, srcs, deps):
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

def yaspl_bootstrap_library(name, srcs, deps=[]):
  yaspl_library(
    name=name,
    srcs=srcs,
    deps=deps,
  )
  _bootstrap_inner(name, srcs, deps)

def yaspl_bootstrap_binary(name, srcs=[], deps=[]):
  yaspl_binary(
    name=name,
    srcs=srcs,
    deps=deps,
  )

  _bootstrap_inner(name, srcs, deps)

  native.filegroup(
    name = name + "_library_files",
    data = [
         name + ".src.list",
         name + ".srcs",
    ],
  )
