load(":yaspl.bzl", "yaspl_library")

filegroup(
  name = "compiler_library_files",
  visibility = ["//visibility:public"],
  srcs = glob(["*.yaspl"])
)

yaspl_library(
  name = "maybe",
  srcs = ["maybe.yaspl"],
)

yaspl_library(
  name = "either",
  srcs = ["either.yaspl"],
)

yaspl_library(
  name = "list",
  srcs = ["list.yaspl"],
  deps = [":maybe"]
)
