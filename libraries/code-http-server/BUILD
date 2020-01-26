load("//libraries:yaspl.bzl", "yaspl_binary", "yaspl_library")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_binary(
    name = "code-http-server",
    srcs = ["code-http-server.yaspl"],
    deps = [
        "//libraries:compiler_lib",
        "//libraries:http-server",
        "//libraries:io",
        "//libraries:ip",
        "//libraries:tcp",
        "//libraries/code-http-server:assembly",
        "//libraries/code-http-server:register",
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:join-list",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/formats:html",
        "//libraries/formats:xml",
        "//libraries/protocols/http:http-response",
        "//libraries/protocols/http:http-responses",
        "//libraries/yaspl:parser",
        "//libraries/yaspl:top-level-objects",
        "//libraries/yaspl:top-level-objects-to-x86-64",
        "//libraries/yaspl:x86-64-assembly",
        "//libraries/yaspl/frontend:errors",
    ],
)

yaspl_library(
    name = "assembly",
    srcs = ["assembly.yaspl"],
    deps = [
        "//libraries/data:bytes",
        "//libraries/data:either",
        "//libraries/data:join-list",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/formats:html",
        "//libraries/formats:xml",
        "//libraries/protocols/http:http-response",
        "//libraries/protocols/http:http-responses",
        "//libraries/yaspl:x86-64-assembly",
        "//libraries/yaspl:x86-64-assembly-printer",
    ],
)

yaspl_library(
    name = "register",
    srcs = ["register.yaspl"],
    deps = [
        "//libraries:panic",
        "//libraries/algorithms:depth-first-search",
        "//libraries/data:bytes",
        "//libraries/data:dict",
        "//libraries/data:indexed-set",
        "//libraries/data:join-list",
        "//libraries/data:lifted-primitives",
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:numbers",
        "//libraries/data:tuples",
        "//libraries/formats:html",
        "//libraries/formats:svg",
        "//libraries/formats:xml",
        "//libraries/prim-language:block-number",
        "//libraries/prim-language:print-register-language",
        "//libraries/prim-language:register-language",
        "//libraries/protocols/http:http-response",
        "//libraries/protocols/http:http-responses",
        "//libraries/yaspl:top-level-objects",
    ],
)

test_suite(
    name = "package_tests",
    testonly = 0,
)

filegroup(
    name = "package_binaries",
    srcs = [
        ":code-http-server",
    ],
)
