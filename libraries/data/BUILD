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
    main_module = "aligned_bytes_test",
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

yaspl_library(
    name = "assoc-test-driver-main",
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
        "//libraries:io",
        "//libraries:sexp-parser",
        "//libraries:sexp-printer",
    ],
)

yaspl_binary(
    name = "assoc-test-driver",
    main_module = "data_assoc_test_driver_main",
    deps = [
        ":assoc-test-driver-main",
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
    main_module = "bytes_test",
    deps = [
        ":bytes",
        ":list",
        ":ordering",
        ":tuples",
        "//libraries:yunit",
    ],
)

yaspl_bootstrap_library(
    name = "dict",
    srcs = ["dict.yaspl"],
    deps = [
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
    main_module = "dict_test",
    deps = [
        ":bytes",
        ":dict",
        ":list",
        ":maybe",
        ":numbers",
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
        ":maybe",
        ":numbers",
        ":tuples",
    ],
)

yaspl_bootstrap_library(
    name = "either",
    srcs = ["either.yaspl"],
)

yaspl_bootstrap_library(
    name = "indexed-set",
    srcs = ["indexed-set.yaspl"],
    deps = [
        ":dict",
        ":list",
        ":maybe",
        ":ordering",
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
    ],
)

yaspl_bootstrap_library(
    name = "list",
    srcs = ["list.yaspl"],
    deps = [
        ":either",
        ":maybe",
        ":ordering",
        ":tuples",
    ],
)

yaspl_test(
    name = "list-test",
    srcs = ["list-test.yaspl"],
    main_module = "list_test",
    deps = [
        ":bytes",
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
    main_module = "maybe_test",
    deps = [
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
        ":list",
        ":maybe",
        ":ordering",
    ],
)

yaspl_bootstrap_library(
    name = "ordering",
    srcs = ["ordering.yaspl"],
)

yaspl_bootstrap_library(
    name = "red-black-tree",
    srcs = ["red-black-tree.yaspl"],
    deps = [
        ":either",
        ":list",
        ":maybe",
        ":ordering",
        ":tuples",
        "//libraries:sexp-parser",
    ],
)

yaspl_test(
    name = "red-black-tree-test",
    size = "small",
    srcs = ["red-black-tree-test.yaspl"],
    main_module = "data_red_black_tree_test",
    deps = [
        ":bytes",
        ":list",
        ":maybe",
        ":numbers",
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
    main_module = "regexp_test",
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
    main_module = "set_test",
    deps = [
        ":list",
        ":numbers",
        ":set",
        "//libraries:yunit",
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
    main_module = "tuples_test",
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
