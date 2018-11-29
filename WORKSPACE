workspace(name = "yaspl")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
  name = "minimal_racket",
  sha256 = "5e92d152248262e80a9ecf2fb072849aafd6efce867ceb54fbb351cf1f160dd4",
  strip_prefix = "minimal-racket-5c25067de0b05849675dcf6ff982d9ee5d6f7106",
  urls = ["https://github.com/endobson/minimal-racket/archive/5c25067de0b05849675dcf6ff982d9ee5d6f7106.tar.gz"]
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
