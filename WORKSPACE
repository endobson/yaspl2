workspace(name = "yaspl")

http_archive(
  name = "minimal_racket",
  sha256 = "9cdf52e420e7a3e1f14033c9db427b6f28f95529f88bb48f1f9422bdfaea3013",
  strip_prefix = "minimal-racket-08cd37aa20aec7065a579130f612fa93af764d81",
  urls = ["https://github.com/endobson/minimal-racket/archive/08cd37aa20aec7065a579130f612fa93af764d81.tar.gz"]
)

load("@minimal_racket//:releases.bzl", "racket_releases")
racket_releases()
