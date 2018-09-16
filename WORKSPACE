workspace(name = "yaspl")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
  name = "minimal_racket",
  sha256 = "dcbd3de1bc301b6decf87f396ad744c49351b414934a28ebfba9502b050e5e6b",
  strip_prefix = "minimal-racket-c9b5cc2cd142eaad21cf3077bd90131ed0252e17",
  urls = ["https://github.com/endobson/minimal-racket/archive/c9b5cc2cd142eaad21cf3077bd90131ed0252e17.tar.gz"]
)
load("@minimal_racket//:releases.bzl", "racket_releases")
racket_releases()
