load("@minimal_racket//:racket.bzl", "racket_binary", "racket_library")

# Macros to reduce boilerplate on simmilar rules in bootstrapping
def bootstrap_binary(name, source_files):
  bootstrapper = "//bootstrap:bootstrapper"
  object_name = name + ".o"
  module_name = name.replace("bootstrap_", "").replace("_", "-") + "-main"
  source_file_list = source_files.replace("_library_files", ".src.list")

  native.genrule(
      name = "gen_" + name,
      outs = [object_name],
      cmd = select({
        "//conditions:darwin":
            "$(execpath %s) --source-list $(execpath %s) --main %s osx $@" %
            (bootstrapper, source_file_list, module_name),
        "//conditions:linux_x86_64":
            "$(execpath %s) --source-list $(execpath %s) --main %s linux $@" %
            (bootstrapper, source_file_list, module_name),
       }),
      srcs = [
          source_files,
          source_file_list,
      ],
      tools = [bootstrapper],
  )

  native.genrule(
      name = "link_" + name,
      srcs = [object_name],
      outs = [name],
      cmd = select({
        "//conditions:host_and_target_darwin":
            "ld -arch x86_64 " +
            "-macosx_version_min 10.11 " +
            "-static " +
            "-no_uuid " +
            "-sectcreate __DATA __data /dev/null " +
            "$(location %s) -o $(location %s) " % (object_name, name) +
            "&& strip -x $(location %s)" % name,
        "//conditions:host_and_target_linux_x86_64":
            "ld $(location %s) -o $(location %s)" % (object_name, name),
       }),
      executable = 1,
      output_to_bindir = 1,
  )
