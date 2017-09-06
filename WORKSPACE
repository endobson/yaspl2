workspace(name = "yaspl")

http_archive(
  name = "minimal_racket",
  sha256 = "d4c6971bfe76f2f0a365adc999383a9b7b37a479112e00ae9032478ca645d131",
  strip_prefix = "minimal-racket-24f0bbac437b5b2b034396b9910af341c16efe80",
  urls = ["https://github.com/endobson/minimal-racket/archive/24f0bbac437b5b2b034396b9910af341c16efe80.tar.gz"]
)

load("@minimal_racket//:releases.bzl", "racket_releases")
racket_releases()
