racket_src_file_type = FileType([".rkt"])
racket_zo_file_type = FileType([".zo"])

# Implementation of racket_binary and racket_test rules
def _bin_impl(ctx):
  script_path = ctx.file.main_module.short_path
  stub_script = (
    "#!/bin/bash\n" +
    'unset PLTCOMPILEDROOTS\n' +
    'exec "${BASH_SOURCE[0]}.runfiles/%s/%s" -U ' % (
       ctx.workspace_name,
       ctx.executable._racket_bin.short_path) +
    '-u "${BASH_SOURCE[0]}.runfiles/%s/%s" "$@"\n'% (
       ctx.workspace_name,
       script_path)
  )

  ctx.file_action(
    output=ctx.outputs.executable,
    content=stub_script,
    executable=True
  )

  runfiles_files = set(ctx.attr._lib_deps.files)

  for target in ctx.attr.deps:
    runfiles_files = runfiles_files | target.racket_transitive_zos

  runfiles = ctx.runfiles(
    transitive_files=runfiles_files,
    collect_data=True,
  )

  return struct(runfiles=runfiles)

def _lib_impl(ctx):
  if (len(ctx.attr.srcs) != 1):
    fail("Must supply exactly one source file: Got %s" % len(ctx.attr.srcs), "srcs")
  src_file = ctx.files.srcs[0]
  src_name = src_file.basename
  if (not(src_name.endswith(".rkt"))):
    fail("Source file must end in .rkt", "srcs")
  if (not(src_name.rpartition(".rkt")[0] == ctx.label.name)):
    fail("Source file must match rule name", "srcs")

  zos = set()
  for target in ctx.attr.deps:
    zos = zos | set(target.racket_transitive_zos)

  arguments = []
  arguments += ["-l", "racket/base"]
  arguments += ["-l", "racket/file"]
  arguments += ["-l", "compiler/compiler"]
  # The file needs to be in the same directory as the .zos because thats how the racket compiler works.
  if (src_file.root != ctx.bin_dir):
    arguments += [
      "-e",
      "(define gen-path (build-path \"%s\" \"%s\"))" %
           (ctx.bin_dir.path, src_file.short_path)]
    arguments += [
      "-e",
      "(begin" +
      "  (make-parent-directory* gen-path) " +
      "  (make-file-or-directory-link (path->complete-path \"%s\") gen-path))" % src_file.path]
    arguments += [
      "-e",
      "((compile-zos #f #:module? #t) (list gen-path) \"%s\")" % ctx.outputs.zo.dirname]

  ctx.action(
    executable=ctx.executable._racket_bin,
    arguments = arguments,
    inputs=ctx.files.srcs + ctx.files._lib_deps + ctx.files.deps + list(zos),
    outputs=[ctx.outputs.zo],
  )

  runfiles_files = set([ctx.outputs.zo])

  for target in ctx.attr.data:
    runfiles_files = runfiles_files | set(target.files)

  for target in ctx.attr.deps:
    runfiles_files = runfiles_files | set(target.files)


  runfiles = ctx.runfiles(
    transitive_files=runfiles_files,
    collect_data=True,
  )

  return struct(
    racket_transitive_zos=list(zos) + [ctx.outputs.zo],
    runfiles=runfiles
  )

_racket_bin_attrs = {
  "main_module": attr.label(
    mandatory=True,
    single_file=True,
    allow_files=racket_src_file_type,
  ),
  "data": attr.label_list(
    allow_files=True,
    cfg="data",
  ),
  "deps": attr.label_list(allow_files=racket_zo_file_type),
  "_lib_deps": attr.label(
    default=Label("@minimal_racket//osx/v6.10:racket-src-osx"),
    cfg="data"
  ),
  "_racket_bin": attr.label(
    default=Label("@minimal_racket//osx/v6.10:bin/racket"),
    executable=True,
    allow_files=True,
    cfg="data"
  ),
}

_racket_lib_attrs = {
  "srcs": attr.label_list(
    allow_files=racket_src_file_type,
    mandatory=True,
    non_empty=True
  ),
  "data": attr.label_list(
    allow_files=True,
    cfg="data",
  ),
  "deps": attr.label_list(
    providers = ["racket_transitive_zos"],
  ),
  "_lib_deps": attr.label(
    default=Label("@minimal_racket//osx/v6.10:racket-src-osx"),
    cfg="host"
  ),
  "_racket_bin": attr.label(
    default=Label("@minimal_racket//osx/v6.10:bin/racket"),
    executable=True,
    allow_files=True,
    cfg="host",
  ),
}

racket_test = rule(
  implementation=_bin_impl,
  test=True,
  executable=True,
  attrs = _racket_bin_attrs
)

racket_binary = rule(
  implementation=_bin_impl,
  executable=True,
  attrs = _racket_bin_attrs
)

racket_library = rule(
  implementation=_lib_impl,
  outputs = {
    "zo": "compiled/%{name}_rkt.zo",
  },
  attrs = _racket_lib_attrs
)
