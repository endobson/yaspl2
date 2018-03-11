workspace(name = "yaspl")

http_archive(
  name = "minimal_racket",
  sha256 = "086679831ff56240881ddd9830dbde8d6ebd2b06181805fe2678c0c1fa6d1238",
  strip_prefix = "minimal-racket-94c6bfbba3c5591e6f33fff09534911dea9cff66",
  urls = ["https://github.com/endobson/minimal-racket/archive/94c6bfbba3c5591e6f33fff09534911dea9cff66.tar.gz"]
)


load("@minimal_racket//:releases.bzl", "racket_releases")
racket_releases()
