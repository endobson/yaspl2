workspace(name = "yaspl")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
  name = "minimal_racket",
  sha256 = "55b0105243f0f52e2d45d94f2018f798c9c882481bb93697e9f10151257a95aa",
  strip_prefix = "minimal-racket-3311f33d15f5992b0246e3c861be42b8518de60b",
  urls = ["https://github.com/endobson/minimal-racket/archive/3311f33d15f5992b0246e3c861be42b8518de60b.tar.gz"]
)
load("@minimal_racket//:releases.bzl", "racket_releases")
racket_releases()

http_archive(
  name = "platforms",
  sha256 = "8d08b89d730e5ef2dfa76ee1aae4ca9e08d770e8b8467ba03c1aa1394b27f616",
  strip_prefix = "platforms-753ad895fe8bb37ab2818d3fd2e9b48c56fc7fde",
  urls = ["https://github.com/bazelbuild/platforms/archive/753ad895fe8bb37ab2818d3fd2e9b48c56fc7fde.tar.gz"],
)

register_toolchains(
    '//libraries/prim-language:osx_prim_language_toolchain',
    '//libraries/prim-language:linux_prim_language_toolchain',
)
register_toolchains(
    '//libraries/yaspl:osx_yaspl_toolchain',
    '//libraries/yaspl:linux_yaspl_toolchain',
)
register_toolchains(
    '@minimal_racket//:osx_racket_bootstrap_toolchain',
    '@minimal_racket//:linux_racket_bootstrap_toolchain',
    '@minimal_racket//:osx_osx_racket_toolchain',
    '@minimal_racket//:linux_linux_racket_toolchain',
    '@minimal_racket//:osx_linux_racket_toolchain',
    '@minimal_racket//:linux_osx_racket_toolchain',
)

http_archive(
  name = "io_bazel_rules_docker",
  sha256 = "aed1c249d4ec8f703edddf35cbe9dfaca0b5f5ea6e4cd9e83e99f3b0d1136c3d",
  strip_prefix = "rules_docker-0.7.0",
  urls = ["https://github.com/bazelbuild/rules_docker/archive/v0.7.0.tar.gz"],
)

load("@io_bazel_rules_docker//repositories:repositories.bzl",
     container_repositories = "repositories")
container_repositories()
