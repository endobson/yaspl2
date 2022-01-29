load("//libraries:yaspl.bzl", "yaspl_provider")

def _runtime_objects_impl(ctx):
  objects = []
  for dep in ctx.attr.deps:
    if yaspl_provider in dep:
      objects.append(dep[yaspl_provider].transitive_objects)
    else:
      objects.append(dep[DefaultInfo].files)

  return [
      DefaultInfo(
        files = depset(transitive = objects)
      )
  ]

runtime_objects = rule(
    implementation = _runtime_objects_impl,
    attrs = {
      "deps": attr.label_list(),
    }
)
