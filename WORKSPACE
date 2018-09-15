workspace(name = "yaspl")

http_archive(
  name = "minimal_racket",
  sha256 = "6eb3553cc6902e557d9f6cf8a70b259fcf68de6a23f2f51896fc7312793120ea",
  strip_prefix = "minimal-racket-b38b24f78c28d2d582715e052730057631ff86fc",
  urls = ["https://github.com/endobson/minimal-racket/archive/b38b24f78c28d2d582715e052730057631ff86fc.tar.gz"]
)


load("@minimal_racket//:releases.bzl", "racket_releases")
racket_releases()
