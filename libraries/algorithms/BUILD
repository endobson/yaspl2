load("//libraries:yaspl.bzl", "yaspl_library", "yaspl_test", "yaspl_bootstrap_library")
package(
  default_visibility = ["//visibility:public"]
)

yaspl_library(
  name = "depth-first-search",
  srcs = ["depth-first-search.yaspl"],
  deps = [
    "//libraries/data:dict",
    "//libraries/data:either",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries/data:tuples",
  ]
)


yaspl_test(
  name = "depth-first-search-test",
  main_module = "depth_first_search_test",
  srcs = ["depth-first-search-test.yaspl"],
  deps = [
    ":depth-first-search",
    "//libraries/data:either",
    "//libraries/data:list",
    "//libraries/data:maybe",
    "//libraries:yunit",
  ]
)