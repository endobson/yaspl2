package(
    default_visibility = ["//visibility:public"],
)

binaries_to_prebuild = {
    "library_compiler": "//bootstrap:bootstrap_library_compiler",
    "main_stub": "//bootstrap:bootstrap_main_stub",
    "linker": "//bootstrap:bootstrap_linker",
    "prim_language_library_compiler": "//bootstrap:bootstrap_prim_language_library_compiler",
    "aspect-missing-imports": "//tools:aspect-missing-imports",
}

[
    alias(
        name = binary_name,
        actual = select({
            ":yaspl_bootstrap": binaries_to_prebuild[binary_name],
            "//conditions:default": ":saved/" + binary_name,
        }),
    )
    for binary_name in binaries_to_prebuild
]

genrule(
    name = "gen_setup_prebuilt_binaries",
    outs = ["setup_prebuilt_binaries"],
    cmd = "printf '%s\n' >>$@ " +
          " ".join([
                       "'#!/bin/bash'",
                       "'set -e'",
                       "'mkdir -p prebuilt/saved'",
                   ] + [
                       "\"cp $(location %s) prebuilt/saved/%s\"" % (target, name)
                       for (name, target) in binaries_to_prebuild.items()
                   ] +
                   [
                       "\"chmod ug+w prebuilt/saved/%s\"" % name
                       for name in binaries_to_prebuild
                   ]),
    executable = True,
    tools = binaries_to_prebuild.values(),
)

config_setting(
    name = "yaspl_bootstrap",
    values = {"define": "yaspl_bootstrap=true"},
)
