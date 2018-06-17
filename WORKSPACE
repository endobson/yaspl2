workspace(name = "yaspl")

http_archive(
  name = "minimal_racket",
  sha256 = "e3881443780584beaa44df8dde957ab67bf9ee61ebc9f52cc6c0b2fd8cffa0cc",
  strip_prefix = "minimal-racket-a2ca2a3e2e6bb6662459ca085e6e0f902a9be34e",
  urls = ["https://github.com/endobson/minimal-racket/archive/a2ca2a3e2e6bb6662459ca085e6e0f902a9be34e.tar.gz"]
)


load("@minimal_racket//:releases.bzl", "racket_releases")
racket_releases()
