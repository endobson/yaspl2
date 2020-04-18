load("@minimal_racket//:racket.bzl", "racket_binary", "racket_library")

# Macros to reduce boilerplate on simmilar rules in bootstrapping
def bootstrap_binary(name, source_files):
  object_name = name + ".o"
  racket_src_name = name + "_rkt.rkt"
  racket_lib_name = name + "_rkt"
  racket_bin_name = name + "_bin"
  short_name = name.replace("bootstrap_", "")
  module_name = short_name.replace("_", "-") + "-main"
  source_file_list = source_files.replace("_library_files", ".src.list")

  racket_contents = """
#lang racket/base

(require
  "bootstrap-compiler.rkt"
  racket/cmdline)

(define source-list* #f)
(define main* #f)
(command-line 
  #:once-each
  ("--source-list" source-list "Relative path to file with list of source files." 
   (set! source-list* source-list))
  ("--main" main "Module name containing main function." 
   (set! main* (string->bytes/utf-8 main)))
  #:args args (current-command-line-arguments (list->vector args)))

(run-bootstrap-compiler source-list* main*)
"""

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
      deps = [
          racket_lib_name,
      ],
  )

  native.genrule(
      name = "gen_" + name,
      outs = [object_name],
      cmd = select({
        "//conditions:darwin":
            "$(execpath %s) --source-list $(execpath %s) --main %s osx $@" %
            (racket_bin_name, source_file_list, module_name),
        "//conditions:linux_x86_64":
            "$(execpath %s) --source-list $(execpath %s) --main %s linux $@" %
            (racket_bin_name, source_file_list, module_name),
       }),
      srcs = [
          source_files,
          source_file_list,
      ],
      tools = [racket_bin_name],
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
