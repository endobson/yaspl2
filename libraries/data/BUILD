load("//libraries:yaspl.bzl", "yaspl_binary", "yaspl_bootstrap_library", "yaspl_library", "yaspl_test")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_bootstrap_library(
    name = "aligned-bytes",
    srcs = ["aligned-bytes.yaspl"],
    deps = [
        ":bytes",
        ":list",
        ":numbers",
    ],
)

yaspl_test(
    name = "aligned-bytes-test",
    srcs = ["aligned-bytes-test.yaspl"],
    deps = [
        ":aligned-bytes",
        ":bytes",
        ":list",
        ":maybe",
        "//libraries:yunit",
    ],
)

yaspl_bootstrap_library(
    name = "assoc-list",
    srcs = ["assoc-list.yaspl"],
    deps = [
        ":list",
        ":maybe",
        ":tuples",
    ],
)

yaspl_binary(
    name = "assoc-test-driver",
    srcs = ["assoc-test-driver-main.yaspl"],
    deps = [
        ":assoc-list",
        ":bytes",
        ":dict",
        ":either",
        ":list",
        ":maybe",
        ":numbers",
        ":red-black-tree",
        ":source-location",
        "//libraries:io",
        "//libraries:sexp-parser",
        "//libraries:sexp-printer",
        "//libraries/formats:sexp",
    ],
)

yaspl_bootstrap_library(
    name = "bytes",
    srcs = ["bytes.yaspl"],
    deps = [
        ":list",
        ":numbers",
        ":ordering",
    ],
)

yaspl_test(
    name = "bytes-test",
    srcs = ["bytes-test.yaspl"],
    deps = [
        ":bytes",
        ":list",
        ":maybe",
        ":ordering",
        ":tuples",
        "//libraries:yunit",
    ],
)

yaspl_bootstrap_library(
    name = "dict",
    srcs = ["dict.yaspl"],
    deps = [
        ":either",
        ":lifted-primitives",
        ":list",
        ":maybe",
        ":ordering",
        ":red-black-tree",
        ":set",
        ":tuples",
    ],
)

yaspl_test(
    name = "dict-test",
    srcs = ["dict-test.yaspl"],
    deps = [
        ":bytes",
        ":dict",
        ":list",
        ":maybe",
        ":set",
        ":tuples",
        "//libraries:yunit",
    ],
)

yaspl_library(
    name = "directed-graph",
    srcs = ["directed-graph.yaspl"],
    deps = [
        ":dict",
        ":lifted-primitives",
        ":maybe",
        ":tuples",
    ],
)

yaspl_bootstrap_library(
    name = "either",
    srcs = ["either.yaspl"],
    deps = [":tuples"],
)

yaspl_bootstrap_library(
    name = "indexed-set",
    srcs = ["indexed-set.yaspl"],
    deps = [
        ":dict",
        ":either",
        ":list",
        ":maybe",
        ":ordering",
        ":set",
        ":tuples",
    ],
)

yaspl_bootstrap_library(
    name = "join-list",
    srcs = ["join-list.yaspl"],
    deps = [":list"],
)

yaspl_bootstrap_library(
    name = "lifted-primitives",
    srcs = ["lifted-primitives.yaspl"],
    deps = [
        ":ordering",
        ":tuples",
    ],
)

yaspl_bootstrap_library(
    name = "list",
    srcs = ["list.yaspl"],
    deps = [
        ":either",
        ":lifted-primitives",
        ":maybe",
        ":ordering",
        ":tuples",
    ],
)

yaspl_bootstrap_library(
    name = "list-multi-dict",
    srcs = ["list-multi-dict.yaspl"],
    deps = [
        ":dict",
        ":list",
        ":maybe",
        ":ordering",
    ],
)

yaspl_test(
    name = "list-test",
    srcs = ["list-test.yaspl"],
    deps = [
        ":lifted-primitives",
        ":list",
        ":maybe",
        ":tuples",
        "//libraries:yunit",
    ],
)

yaspl_bootstrap_library(
    name = "maybe",
    srcs = ["maybe.yaspl"],
)

yaspl_test(
    name = "maybe-test",
    srcs = ["maybe-test.yaspl"],
    deps = [
        ":bytes",
        ":list",
        ":maybe",
        "//libraries:yunit",
    ],
)

yaspl_bootstrap_library(
    name = "multi-set",
    srcs = ["multi-set.yaspl"],
    deps = [
        ":dict",
        ":lifted-primitives",
        ":list",
        ":maybe",
        ":ordering",
        ":set",
    ],
)

yaspl_bootstrap_library(
    name = "numbers",
    srcs = ["numbers.yaspl"],
    deps = [
        ":lifted-primitives",
        ":maybe",
        ":ordering",
    ],
)

yaspl_bootstrap_library(
    name = "ordering",
    srcs = ["ordering.yaspl"],
)

yaspl_bootstrap_library(
    name = "range-map",
    srcs = ["range-map.yaspl"],
    deps = [
        ":lifted-primitives",
        ":maybe",
        ":red-black-tree",
        ":tuples",
    ],
)

yaspl_test(
    name = "range-map-test",
    size = "small",
    srcs = ["range-map-test.yaspl"],
    deps = [
        ":bytes",
        ":list",
        ":maybe",
        ":range-map",
        ":tuples",
        "//libraries:yunit",
    ],
)

yaspl_bootstrap_library(
    name = "red-black-tree",
    srcs = ["red-black-tree.yaspl"],
    deps = [
        ":either",
        ":lifted-primitives",
        ":list",
        ":maybe",
        ":ordering",
        ":tuples",
    ],
)

yaspl_test(
    name = "red-black-tree-test",
    size = "small",
    srcs = ["red-black-tree-test.yaspl"],
    deps = [
        ":bytes",
        ":list",
        ":maybe",
        ":red-black-tree",
        ":tuples",
        "//libraries:yunit",
    ],
)

yaspl_library(
    name = "regexp",
    srcs = ["regexp.yaspl"],
    deps = [
        ":bytes",
        ":lifted-primitives",
        ":list",
        ":maybe",
        ":tuples",
    ],
)

yaspl_test(
    name = "regexp-test",
    size = "small",
    srcs = ["regexp-test.yaspl"],
    deps = [
        ":list",
        ":maybe",
        ":regexp",
        "//libraries:yunit",
    ],
)

yaspl_bootstrap_library(
    name = "set-multi-dict",
    srcs = ["set-multi-dict.yaspl"],
    deps = [
        ":dict",
        ":maybe",
        ":ordering",
        ":set",
    ],
)

yaspl_bootstrap_library(
    name = "set",
    srcs = ["set.yaspl"],
    deps = [
        ":lifted-primitives",
        ":list",
        ":maybe",
        ":ordering",
        ":red-black-tree",
        ":tuples",
    ],
)

yaspl_test(
    name = "set-test",
    size = "small",
    srcs = ["set-test.yaspl"],
    deps = [
        ":bytes",
        ":list",
        ":maybe",
        ":set",
        "//libraries:yunit",
    ],
)

yaspl_bootstrap_library(
    name = "source-location",
    srcs = ["source-location.yaspl"],
    deps = [
        ":bytes",
        ":list",
        ":numbers",
        ":tuples",
    ],
)

yaspl_bootstrap_library(
    name = "tuples",
    srcs = ["tuples.yaspl"],
    deps = [
        ":ordering",
    ],
)

yaspl_test(
    name = "tuples-test",
    srcs = ["tuples-test.yaspl"],
    deps = [
        ":bytes",
        ":list",
        ":maybe",
        ":tuples",
        "//libraries:yunit",
    ],
)

yaspl_bootstrap_library(
    name = "undirected-graph",
    srcs = ["undirected-graph.yaspl"],
    deps = [
        ":dict",
        ":either",
        ":maybe",
        ":ordering",
        ":tuples",
    ],
)

yaspl_bootstrap_library(
    name = "unique-dict",
    srcs = ["unique-dict.yaspl"],
    deps = [
        ":dict",
        ":maybe",
        ":ordering",
        ":set",
    ],
)

test_suite(
    name = "package_tests",
    testonly = 0,
)

filegroup(
    name = "package_binaries",
    srcs = [
        ":assoc-test-driver",
    ],
)
