workspace(name = "yaspl")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive", "http_file")

http_archive(
  name = "minimal_racket",
  sha256 = "63acf0d9e6a201d5a93803cfd372b760b71d3271f05f317d6b85029e3a4403be",
  strip_prefix = "minimal-racket-3ba0f3f887da943326085904e05b8cd1ea8296b0",
  urls = ["https://github.com/endobson/minimal-racket/archive/3ba0f3f887da943326085904e05b8cd1ea8296b0.tar.gz"]
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
    '//libraries/core-language:osx_core_language_toolchain',
    '//libraries/core-language:linux_core_language_toolchain',
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

http_file(
  name = "ovmf_code",
  sha256 = "81ede7af3351a1710a4d4a35655fac10bedb11ae3484ae785820803a5bf69829",
  urls = ["https://github.com/clearlinux/common/raw/9749d00c562b5852eb192567335f82eca434c4be/OVMF_CODE.fd"],
)
http_file(
  name = "ovmf_vars",
  sha256 = "5d2ac383371b408398accee7ec27c8c09ea5b74a0de0ceea6513388b15be5d1e",
  urls = ["https://github.com/clearlinux/common/raw/9749d00c562b5852eb192567335f82eca434c4be/OVMF_VARS.fd"],
)
