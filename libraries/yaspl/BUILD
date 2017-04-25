load("//libraries:yaspl.bzl", "yaspl_library", "yaspl_binary", "yaspl_test",
     "yaspl_bootstrap_library")
package(
  default_visibility = ["//visibility:public"]
)

yaspl_bootstrap_library(
  name = "x86-64-runtime",
  srcs = ["x86-64-runtime.yaspl"],
  deps = [
    "//libraries:x86-64-stack-machine",
    "//libraries/data:bytes",
    "//libraries/data:join-list",
    "//libraries/data:list",
  ]
)
