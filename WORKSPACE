workspace(name = "yaspl")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
  name = "minimal_racket",
  sha256 = "a4fbee7140065dbcf2c9e8d181a031a4c2c272ee5731006799a5ca69fc116dc6",
  strip_prefix = "minimal-racket-a98ed78cbe60699ec954304e6346c3f4433ee330",
  urls = ["https://github.com/endobson/minimal-racket/archive/a98ed78cbe60699ec954304e6346c3f4433ee330.tar.gz"]
)
load("@minimal_racket//:releases.bzl", "racket_releases")
racket_releases()

register_toolchains(
    '//libraries/prim-language:osx_prim_language_toolchain',
    '//libraries/prim-language:linux_prim_language_toolchain',
)
register_toolchains(
    '//libraries/yaspl:osx_yaspl_toolchain',
    '//libraries/yaspl:linux_yaspl_toolchain',
)
register_toolchains(
    '@minimal_racket//:osx_osx_racket_toolchain',
    '@minimal_racket//:linux_linux_racket_toolchain',
    '@minimal_racket//:osx_linux_racket_toolchain',
    '@minimal_racket//:linux_osx_racket_toolchain',
)
