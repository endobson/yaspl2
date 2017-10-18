workspace(name = "yaspl")

http_archive(
  name = "minimal_racket",
  sha256 = "a6e4607f270465455f5d62480f805fde236b60762411944e24f62ebf260a4c81",
  strip_prefix = "minimal-racket-6a920d11d3b3ffb6b4b108fbaf01846bec150882",
  urls = ["https://github.com/endobson/minimal-racket/archive/6a920d11d3b3ffb6b4b108fbaf01846bec150882.tar.gz"]
)


load("@minimal_racket//:releases.bzl", "racket_releases")
racket_releases()
