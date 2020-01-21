load("//libraries:yaspl.bzl", "yaspl_provider")
load(":yaspl-transitive-rules.bzl", "transitivify_impl", "transitive_attrs",
     "concat_files_impl")

yaspl_module_dependency_graph_provider = provider(fields=["files"])

def _yaspl_library_module_dependency_graph_impl(target, ctx):
  target_yaspl_provider = target[yaspl_provider]
  input_signatures = target_yaspl_provider.input_signatures
  source_file = target_yaspl_provider.source_file

  if not source_file.is_source:
    return yaspl_module_dependency_graph_provider(files=depset())

  output_file = ctx.actions.declare_file(ctx.label.name + ".module-dependencies")
  args = ctx.actions.args()
  args.add_all(input_signatures)

  ctx.actions.run_shell(
     outputs = [output_file],
     inputs = [source_file],
     tools = [ctx.executable._module_dependency_graph],
     command = '%s %s $@ >%s' %
         (ctx.executable._module_dependency_graph.path, source_file.path, output_file.path),
  )

  return yaspl_module_dependency_graph_provider(files=depset([output_file]))

def _yaspl_module_dependency_graph_impl(target, ctx):
  return [transitivify_impl(target, ctx, _yaspl_library_module_dependency_graph_impl,
                                    yaspl_module_dependency_graph_provider)]

yaspl_module_dependency_graph = aspect(
  implementation = _yaspl_module_dependency_graph_impl,
  attr_aspects = transitive_attrs,
  provides = [yaspl_module_dependency_graph_provider],
  attrs = {
    "_module_dependency_graph": attr.label(
       default=Label("//tools:aspect-module-dependency-graph"),
       executable=True,
       allow_files=True,
       cfg="host",
    ),
  }
)

def _yaspl_module_dependency_graph_rule_impl(ctx):
  return concat_files_impl(ctx, yaspl_module_dependency_graph_provider,
                           prefix='strict digraph {\nrankdir=LR;\n',
                           suffix='}\n')

yaspl_module_dependency_graph_rule = rule(
  implementation = _yaspl_module_dependency_graph_rule_impl,
  attrs = {
    "deps": attr.label_list(aspects=[yaspl_module_dependency_graph]),
  },
  outputs = {
    "combined": "%{name}.module-dependency-graph"
  },
)
