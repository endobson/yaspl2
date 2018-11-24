# Macros to reduce boilerplate on simmilar rules in bootstrapping
def bootstrap_binary(name, generator):
  assembly_name = name + ".s"
  object_name = name + ".o"

  native.genrule(
      name = "gen_" + name,
      outs = [assembly_name],
      cmd = select({
        "//conditions:darwin":
            "$(location %s) osx-assembly $@" % generator,
        "//conditions:linux_x86_64":
            "$(location %s) linux-assembly $@" % generator,
       }),
      tools = [generator],
  )

  native.genrule(
      name = "assemble_" + name,
      srcs = [assembly_name],
      outs = [object_name],
      cmd = select({
        "//conditions:host_and_target_darwin":
           "as $(location %s) -o $(location %s)"
           % (assembly_name, object_name),
        "//conditions:host_and_target_linux_x86_64":
           "as $(location %s) -o $(location %s)"
           % (assembly_name, object_name),
       }),
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
            "$(location %s) -o $(location %s)" % (object_name, name),
        "//conditions:host_and_target_linux_x86_64":
            "ld $(location %s) -o $(location %s)" % (object_name, name),
       }),
      executable = 1,
      output_to_bindir = 1,
  )