package(
    default_visibility = ["//visibility:public"],
)

filegroup(
    name = "library_compiler",
    srcs = select({
        ":yaspl_bootstrap": ["//bootstrap:bootstrap_library_compiler"],
        "//conditions:default": [":saved/library_compiler"],
    }),
)

filegroup(
    name = "main_stub",
    srcs = select({
        ":yaspl_bootstrap": ["//bootstrap:bootstrap_main_stub"],
        "//conditions:default": [":saved/main_stub"],
    }),
)

filegroup(
    name = "linker",
    srcs = select({
        ":yaspl_bootstrap": ["//bootstrap:bootstrap_linker"],
        "//conditions:default": [":saved/linker"],
    }),
)

filegroup(
    name = "prim_language_library_compiler",
    srcs = select({
        ":yaspl_bootstrap": ["//bootstrap:bootstrap_prim_language_library_compiler"],
        "//conditions:default": [":saved/prim_language_library_compiler"],
    }),
)

genrule(
    name = "build_prebuilt_binaries",
    outs = ["prebuilt_binaries"],
    cmd = "touch $@",
    tools = [
        "//bootstrap:bootstrap_library_compiler",
        "//bootstrap:bootstrap_linker",
        "//bootstrap:bootstrap_main_stub",
        "//bootstrap:bootstrap_prim_language_library_compiler",
    ],
)

config_setting(
    name = "yaspl_bootstrap",
    values = {"define": "yaspl_bootstrap=true"},
)
