load("//libraries:yaspl.bzl", "yaspl_library")

def _raw_yaspl_embed_file_impl(ctx):
  if (len(ctx.files.srcs) != 1):
    fail("Only one source is supported", "srcs")
  src_file = ctx.files.srcs[0]

  args = ctx.actions.args()
  args.add(ctx.attr.module_name)
  args.add(src_file)
  args.add(ctx.outputs.output)

  ctx.actions.run(
      inputs = [src_file],
      outputs = [ctx.outputs.output],
      mnemonic = "YasplEmbed",
      executable = ctx.executable._embedder,
      arguments = [args],
  )

  return []


raw_yaspl_embed_file = rule(
    implementation = _raw_yaspl_embed_file_impl,
    attrs = {
      "srcs": attr.label_list(
        allow_files=True,
      ),
      "module_name": attr.string(),
      "output": attr.output(),
      "_embedder": attr.label(
         default=Label("//rules:embedder"),
         executable=True,
         allow_files=True,
         cfg="exec",
      ),
    },
)

def yaspl_embed_file(name, srcs, module_name):
  gen_file = name + ".yaspl"

  raw_yaspl_embed_file(
    name = name + "_gen",
    srcs = srcs,
    module_name = module_name,
    output = gen_file
  )

  yaspl_library(
    name = name,
    srcs = [gen_file]
  )
    

