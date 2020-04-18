load("@minimal_racket//:racket.bzl", "racket_binary", "racket_library")

# Macros to reduce boilerplate on similar rules in bootstrapping
def bootstrap_binary(name, source_files):
  bootstrapper = "//bootstrap:bootstrapper"
  object_name = name + ".o"
  module_name = name.replace("bootstrap_", "").replace("_", "-") + "-main"
  source_file_list = source_files.replace("_library_files", ".src.list")
  os = select({
    "@bazel_tools//src/conditions:darwin": "osx",
    "@bazel_tools//src/conditions:linux_x86_64": "linux",
  })

  native.genrule(
      name = "gen_" + name,
      outs = [object_name, name],
      cmd = "$(execpath %s) --source-list $(execpath %s) --main %s " %
            (bootstrapper, source_file_list, module_name) + os + 
            " $(execpath %s) $(execpath %s)" % (object_name, name),
      srcs = [
          source_files,
          source_file_list,
      ],
      tools = [bootstrapper],
  )
