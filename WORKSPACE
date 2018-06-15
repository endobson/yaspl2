workspace(name = "yaspl")

http_archive(
  name = "minimal_racket",
  sha256 = "8d66accdd10ca397bf51280c6cd5a8320b9fc4311a590265841ec104d8f1393d",
  strip_prefix = "minimal-racket-3130fe214b6ed4bc4cd39e1709609e4472395255",
  urls = ["https://github.com/endobson/minimal-racket/archive/3130fe214b6ed4bc4cd39e1709609e4472395255.tar.gz"]
)


load("@minimal_racket//:releases.bzl", "racket_releases")
racket_releases()
