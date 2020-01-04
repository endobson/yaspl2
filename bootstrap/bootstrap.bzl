load("@minimal_racket//:racket.bzl", "racket_binary", "racket_library")

# Macros to reduce boilerplate on simmilar rules in bootstrapping
def bootstrap_binary(name, source_files):
  assembly_name = name + ".s"
  object_name = name + ".o"
  racket_src_name = name + "_rkt.rkt"
  racket_lib_name = name + "_rkt"
  racket_bin_name = name + "_bin"
  short_name = name.replace("bootstrap_", "")
  source_file_list = (source_files
   .replace("//", "../")
   .replace(":", "/")
   .replace("_library_files", ".src.list"))

  racket_contents = """
#lang racket/base

(require
  "bootstrap-compiler.rkt"
  racket/runtime-path)

(define-runtime-path library-compiler-list-file "%s")
(run-bootstrap-compiler library-compiler-list-file #"%s-main")
""" % (source_file_list, short_name.replace("_", "-"))

  native.genrule(
      name = "racketize_" + name,
      outs = [racket_src_name],
      cmd = "echo >>$@ '%s'" % racket_contents,
  )

  racket_library(
      name = racket_lib_name,
      srcs = [racket_src_name],
      deps = [
          "//bootstrap:bootstrap-compiler"
      ],
  )

  racket_binary(
      name = racket_bin_name,
      main_module = racket_src_name,
      data = [
          source_files,
      ],
      deps = [
          racket_lib_name,
      ],
  )


  native.genrule(
      name = "gen_" + name,
      outs = [assembly_name],
      cmd = select({
        "//conditions:darwin":
            "$(location %s) assembly-osx $@" % racket_bin_name,
        "//conditions:linux_x86_64":
            "$(location %s) assembly-linux $@" % racket_bin_name,
       }),
      tools = [racket_bin_name],
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
            "$(location %s) -o $(location %s) " % (object_name, name) +
            "&& strip -x $(location %s)" % name,
        "//conditions:host_and_target_linux_x86_64":
            "ld $(location %s) -o $(location %s)" % (object_name, name),
       }),
      executable = 1,
      output_to_bindir = 1,
  )
