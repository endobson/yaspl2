yaspl_provider = provider(fields = ["source_file", "module_name_file", "input_signatures",
                                    "signature", "transitive_objects"])
yaspl_src_provider = provider(fields = ["files"])

def _lib_impl(ctx):
  if (len(ctx.files.srcs) != 1):
    fail("Only one source is supported", "srcs")
  src_file = ctx.files.srcs[0]

  output_object = ctx.actions.declare_file("%s.o" % ctx.attr.name)
  output_signature = ctx.actions.declare_file("%s.sig" % ctx.attr.name)
  output_module_name = ctx.actions.declare_file("%s.module_name" % ctx.attr.name)

  toolchain = ctx.toolchains["//libraries/yaspl:yaspl_toolchain"]
  input_signatures = [dep[yaspl_provider].signature for dep in ctx.attr.deps]

  args = ctx.actions.args()
  args.add(toolchain.platform)
  args.add(output_object)
  args.add(output_signature)
  args.add(src_file)
  args.add_all(input_signatures)

  ctx.actions.run(
    inputs = input_signatures + ctx.files.srcs,
    outputs = [output_object, output_signature],
    mnemonic = "YasplCompile",
    executable = ctx.executable._library_compiler,
    arguments = [args],
  )

  ctx.actions.run_shell(
    inputs = ctx.files.srcs,
    outputs = [output_module_name],
    command = 'echo "$(head -n 1 %s | sed -e "s/#:module //")" > %s'
       % (src_file.path, output_module_name.path),
  )

  return [
    yaspl_provider(
      source_file = src_file,
      module_name_file = output_module_name,
      signature = output_signature,
      input_signatures = input_signatures,
      transitive_objects = depset(
        direct = [output_object],
        transitive = [dep[yaspl_provider].transitive_objects for dep in ctx.attr.deps]
      ),
    ),
    DefaultInfo(
      files = depset([output_object])
    ),
  ]

def _src_impl(ctx):
  output_list = ctx.outputs.list

  transitive_srcs = depset(
    direct = ctx.files.srcs,
    transitive = [dep[yaspl_src_provider].files for dep in ctx.attr.deps],
  )
  args = ctx.actions.args()
  args.add_all(transitive_srcs)

  ctx.actions.run_shell(
    outputs = [output_list],
    command = 'printf "%%s\n" "$@" > %s' % output_list.path,
    arguments = [args],
  )

  return [
    DefaultInfo(
      files = depset(direct = [output_list], transitive = [transitive_srcs]),
      runfiles = ctx.runfiles(
        files = [output_list],
        transitive_files = transitive_srcs,
      ),
    ),
    yaspl_src_provider(files = transitive_srcs),
  ]

def _bin_impl(ctx):
  if (len(ctx.attr.deps) != 1):
    fail("Only one dep is supported", "deps")
  dep = ctx.attr.deps[0]

  output_main_stub = ctx.actions.declare_file("%s_main.o" % ctx.attr.name)

  toolchain = ctx.toolchains["//libraries/yaspl:yaspl_toolchain"]

  args = ctx.actions.args()
  args.add(toolchain.platform)
  args.add(output_main_stub)
  args.add(dep[yaspl_provider].signature)

  ctx.actions.run(
    inputs = [dep[yaspl_provider].signature],
    tools = [ctx.executable._main_stub],
    outputs = [output_main_stub],
    mnemonic = "YasplGenerateMain",
    executable = ctx.executable._main_stub,
    arguments = [args],
  )

  input_objects = depset(
    direct = [output_main_stub],
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
    DefaultInfo(
      files = depset([ctx.outputs.executable])
    ),
  ]

def _link_impl(ctx):
  toolchain = ctx.toolchains["//libraries/yaspl:yaspl_toolchain"]

  input_objects = depset(
    direct = [obj for obj in ctx.files.objects],
    transitive = [dep[yaspl_provider].transitive_objects for dep in ctx.attr.deps],
  )

  args = ctx.actions.args()
  args.add(toolchain.platform)
  args.add(ctx.outputs.output)
  args.add_all(input_objects)

  ctx.actions.run(
    inputs = input_objects,
    tools = [ctx.executable._linker],
    outputs = [ctx.outputs.output],
    mnemonic = "YasplLink",
    executable = ctx.executable._linker,
    arguments = [args],
  )
  return [
    DefaultInfo(
      files = depset([ctx.outputs.output])
    ),
  ]


_yaspl_src_file_extensions = [".yaspl"]

_bootstrap_library_compiler = attr.label(
  default=Label("//prebuilt:library_compiler"),
  executable=True,
  allow_files=True,
  cfg="exec",
)

_bootstrap_main_stub = attr.label(
  default=Label("//prebuilt:main_stub"),
  executable=True,
  allow_files=True,
  cfg="exec",
)

_bootstrap_linker = attr.label(
  default=Label("//prebuilt:linker"),
  executable=True,
  allow_files=True,
  cfg="exec",
)

_yaspl_runtime_objects = attr.label(
  default=Label("//libraries/yaspl/runtime:runtime"),
)


yaspl_library = rule(
  implementation = _lib_impl,
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
  attrs = {
    "srcs": attr.label_list(
      allow_files=_yaspl_src_file_extensions,
    ),
    "list": attr.output(),
    "deps": attr.label_list(
      providers = [yaspl_src_provider],
    )
  },
)

yaspl_link = rule(
  implementation = _link_impl,
  attrs = {
    "output": attr.output(),
    "deps": attr.label_list(
      providers = [yaspl_provider],
    ),
    "objects": attr.label_list(
      allow_files = [".o"],
    ),
    "_linker": _bootstrap_linker,
  },
  toolchains = ["//libraries/yaspl:yaspl_toolchain"],
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

def _yaspl_make_variables_impl(ctx):
  toolchain = ctx.toolchains["//libraries/yaspl:yaspl_toolchain"]

  return [
    platform_common.TemplateVariableInfo({
        "YASPL_PLATFORM": toolchain.platform,
    }),
  ]

yaspl_make_variables = rule(
  implementation = _yaspl_make_variables_impl,
  toolchains = ["//libraries/yaspl:yaspl_toolchain"],
)

def yaspl_test(name, srcs=[], deps=[], size="medium", tags = []):
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
    tags = tags,
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
    list=name + ".src.list",
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
    srcs = [
         name + ".src",
         name + ".srcs",
    ],
  )
