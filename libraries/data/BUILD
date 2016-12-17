load("//libraries:yaspl.bzl", "yaspl_library", "yaspl_test", "yaspl_bootstrap_library")
package(
  default_visibility = ["//visibility:public"]
)

yaspl_bootstrap_library(
  name = "boolean",
  srcs = ["boolean.yaspl"],
)

yaspl_bootstrap_library(
  name = "bytes",
  srcs = ["bytes.yaspl"],
  deps = [
    ":list"
  ]
)

yaspl_bootstrap_library(
  name = "dict",
  srcs = ["dict.yaspl"],
  deps = [
    ":list",
    ":maybe",
    ":tuples",
  ]
)

yaspl_bootstrap_library(
  name = "either",
  srcs = ["either.yaspl"],
)

yaspl_bootstrap_library(
  name = "join-list",
  srcs = ["join-list.yaspl"],
  deps = [":list"]
)

yaspl_bootstrap_library(
  name = "list",
  srcs = ["list.yaspl"],
  deps = [
    ":either",
    ":maybe"
  ]
)

yaspl_bootstrap_library(
  name = "maybe",
  srcs = ["maybe.yaspl"],
)

yaspl_bootstrap_library(
  name = "multi-set",
  srcs = ["multi-set.yaspl"],
  deps = [
    ":dict",
  ]
)

yaspl_bootstrap_library(
  name = "numbers",
  srcs = ["numbers.yaspl"],
)

yaspl_library(
  name = "regexp",
  srcs = ["regexp.yaspl"],
  deps = [
    ":list",
    ":maybe",
    ":tuples",
    ":bytes",
  ]
)

yaspl_bootstrap_library(
  name = "set",
  srcs = ["set.yaspl"],
  deps = [
    ":list",
  ]
)

yaspl_test(
  name = "set-test",
  main_module = "set_test",
  srcs = ["set-test.yaspl"],
  deps = [
    ":boolean",
    ":list",
    ":maybe",
    ":set",
    "//libraries:yunit",
  ]
)

yaspl_bootstrap_library(
  name = "tuples",
  srcs = ["tuples.yaspl"],
)
