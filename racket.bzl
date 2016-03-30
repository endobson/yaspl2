# Compute the short path of the racket binary
def _racket_binary_path(ctx):
  return list(ctx.attr._racket_deps.files)[0].short_path

racket_src_file_type = FileType([".rkt"])

# Implementation of racket_binary and racket_test rules
def _bin_impl(ctx):
  if (len(ctx.attr.srcs) != 1):
    fail("Must supply exactly one source file: Got %s" % len(ctx.attr.srcs), "srcs")

  script_path = list(ctx.attr.srcs[0].files)[0].short_path
  stub_script = (
    "#!/bin/bash\n" +
    'pushd . > /dev/null\n' +
    'SCRIPT_DIR="${BASH_SOURCE[0]}";\n' +
    'while([ -h "${SCRIPT_DIR}" ]); do\n' +
    'cd "`dirname "${SCRIPT_DIR}"`"\n' +
    'SCRIPT_DIR="$(readlink "`basename "${SCRIPT_DIR}"`")";\n' +
    'done\n' +
    'cd "`dirname "${SCRIPT_DIR}"`" > /dev/null\n' +
    'SCRIPT_DIR="`pwd`";\n' +
    'popd  > /dev/null\n' +
    'unset PLTCOMPILEDROOTS\n' +
    'exec "$SCRIPT_DIR/%s.runfiles/%s" -U ' % (
       ctx.label.name,
       _racket_binary_path(ctx)) +
    '-u "$SCRIPT_DIR/%s.runfiles/%s" "$@"\n'% (
       ctx.label.name,
       script_path)
  )


  ctx.file_action(
    output=ctx.outputs.executable,
    content=stub_script,
    executable=True
  )

  runfiles_files = set()

  runfiles_files = runfiles_files + set(ctx.attr._lib_deps.files)
  runfiles_files = runfiles_files + set(ctx.attr._rackunit_deps.files)
  for target in ctx.attr.srcs:
    runfiles_files = runfiles_files | set(target.files)
  for target in ctx.attr.data:
    runfiles_files = runfiles_files | set(target.files)

  for target in ctx.attr.deps:
    runfiles_files = runfiles_files | set(target.files)


  runfiles = ctx.runfiles(
    transitive_files=runfiles_files,
    collect_data=True,
  )

  return struct(runfiles=runfiles)


def _lib_impl(ctx):
  if (len(ctx.attr.srcs) != 1):
    fail("Must supply exactly one source file: Got %s" % len(ctx.attr.srcs), "srcs")
  src_name = ctx.files.srcs[0].basename
  if (not(src_name.endswith(".rkt"))):
    fail("Source file must end in .rkt", "srcs")
  if (not(src_name.rpartition(".rkt")[0] == ctx.label.name)):
    fail("Source file must match rule name", "srcs")


  gen_rkt = ctx.new_file("%s.gen.rkt" % ctx.label.name)


  ctx.template_action(
    template=ctx.files.srcs[0],
    output=gen_rkt,
    substitutions={}
  )

  ctx.action(
    executable=ctx.files._racket_deps[0],
    arguments = [
      "-l", "racket/base",
      "-l", "compiler/compiler",
      "-e",
      "((compile-zos #f #:module? #t)" +
      " (list \"%s\")" % gen_rkt.path +
      " \"%s\"" % ctx.outputs.zo.dirname + ")",
      "-e",
      "(rename-file-or-directory " +
      "  \"%s\"" % ctx.outputs.zo.path.replace("_rkt.zo", ".gen_rkt.zo") +
      "  \"%s\")" % ctx.outputs.zo.path
    ],
    inputs=[ctx.files.srcs[0], gen_rkt] + ctx.files.deps,
    outputs=[ctx.outputs.zo],
  )


_base_racket_attrs = {
  "srcs": attr.label_list(
    allow_files=racket_src_file_type,
    mandatory=True,
    non_empty=True
  ),
  "data": attr.label_list(
    allow_files=True,
    cfg=DATA_CFG
  ),
  "deps": attr.label_list(allow_files=racket_src_file_type),
  "_lib_deps": attr.label(default=Label("@minimal_racket//osx/v6.4:racket-src-osx")),
  "_rackunit_deps": attr.label(default=Label("@minimal_racket//osx/v6.4:rackunit")),
  "_racket_deps": attr.label(
    default=Label("@minimal_racket//osx/v6.4:bin/racket"),
    executable=True,
    allow_files=True
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
    cfg=DATA_CFG
  ),
  "deps": attr.label_list(),
  "_lib_deps": attr.label(default=Label("@minimal_racket//osx/v6.4:racket-src-osx")),
  "_rackunit_deps": attr.label(default=Label("@minimal_racket//osx/v6.4:rackunit")),
  "_racket_deps": attr.label(
    default=Label("@minimal_racket//osx/v6.4:bin/racket"),
    executable=True,
    allow_files=True
  ),
}



racket_test = rule(
  implementation=_bin_impl,
  test=True,
  executable=True,
  attrs = _base_racket_attrs
)

racket_binary = rule(
  implementation=_bin_impl,
  executable=True,
  attrs = _base_racket_attrs
)

racket_library = rule(
  implementation=_lib_impl,
  outputs = {
    "zo": "compiled/%{name}_rkt.zo",
  },
  attrs = _racket_lib_attrs
)
