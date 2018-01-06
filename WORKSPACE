workspace(name = "yaspl")

http_archive(
  name = "minimal_racket",
  sha256 = "d00c6b1192db3a882efdf2d4d27b1777f5483004d269132ac1776e4296829814",
  strip_prefix = "minimal-racket-987a4bae513585f8d2c62d6a61eadd2217757f87",
  urls = ["https://github.com/endobson/minimal-racket/archive/987a4bae513585f8d2c62d6a61eadd2217757f87.tar.gz"]
)


load("@minimal_racket//:releases.bzl", "racket_releases")
racket_releases()
