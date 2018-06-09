load("//libraries:yaspl.bzl", "yaspl_bootstrap_library", "yaspl_test")

package(
    default_visibility = ["//visibility:public"],
)

yaspl_bootstrap_library(
    name = "depth-first-search",
    srcs = ["depth-first-search.yaspl"],
    deps = [
        "//libraries/data:list",
        "//libraries/data:ordering",
        "//libraries/data:set",
        "//libraries/data:tuples",
    ],
)

yaspl_test(
    name = "depth-first-search-test",
    srcs = ["depth-first-search-test.yaspl"],
    main_module = "depth_first_search_test",
    deps = [
        ":depth-first-search",
        "//libraries:yunit",
        "//libraries/data:bytes",
        "//libraries/data:list",
        "//libraries/data:maybe",
    ],
)

yaspl_bootstrap_library(
    name = "find-chains",
    srcs = ["find-chains.yaspl"],
    deps = [
        "//libraries/data:list",
        "//libraries/data:maybe",
        "//libraries/data:set",
        "//libraries/data:unique-dict",
    ],
)
