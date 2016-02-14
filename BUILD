load(":racket.bzl", "racket_test")

racket_test(
  name = "tests_rkt",
  srcs = [
    "tests.rkt",
  ],
  data = glob(["libraries/*.yaspl", "tests/*"]),
  deps = glob(["*.rkt"]),
)
