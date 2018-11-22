workspace(name = "yaspl")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
  name = "minimal_racket",
  sha256 = "be730207d2aea33f0651569351a7ee0cde8dc9fd5d7ae0f0157c370fdd5cb38a",
  strip_prefix = "minimal-racket-b54d4aae1de462c272d98c0fadb45fc45abaca57",
  urls = ["https://github.com/endobson/minimal-racket/archive/b54d4aae1de462c272d98c0fadb45fc45abaca57.tar.gz"]
)
load("@minimal_racket//:releases.bzl", "racket_releases")
racket_releases()

register_toolchains(
    '//libraries/prim-language:osx_prim_language_toolchain',
    '//libraries/prim-language:linux_prim_language_toolchain',
)
register_toolchains(
    '@minimal_racket//:osx_racket_toolchain',
    '@minimal_racket//:linux_racket_toolchain',
)
