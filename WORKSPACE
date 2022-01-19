workspace(name = "yaspl")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive", "http_file")

http_archive(
  name = "minimal_racket",
  sha256 = "e0443c43edfc1381fb0bdbf394e24cd505f2d86923409a6d223581be40c5edc7",
  strip_prefix = "minimal-racket-3c95378d2c5379225fd8a233988773d97c9c4fc4",
  urls = ["https://github.com/endobson/minimal-racket/archive/3c95378d2c5379225fd8a233988773d97c9c4fc4.tar.gz"]
)
load("@minimal_racket//:releases.bzl", "racket_releases")
racket_releases()

http_archive(
  name = "platforms",
  sha256 = "6b0c44197c210f892b4883f4d3ab861943c4e35ac4bf00632d5a62faf90eb925",
  strip_prefix = "platforms-540032531bff7331d737277a41a3e923655dd45c",
  urls = ["https://github.com/bazelbuild/platforms/archive/540032531bff7331d737277a41a3e923655dd45c.tar.gz"],
)

register_toolchains(
    '//libraries/core-language:osx_core_language_toolchain',
    '//libraries/core-language:linux_core_language_toolchain',
    '//libraries/core-language:yasplos_core_language_toolchain',
)
register_toolchains(
    '//libraries/prim-language:osx_prim_language_toolchain',
    '//libraries/prim-language:linux_prim_language_toolchain',
)
register_toolchains(
    '//libraries/yaspl:osx_yaspl_toolchain',
    '//libraries/yaspl:linux_yaspl_toolchain',
    '//libraries/yaspl:yasplos_yaspl_toolchain',
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
    sha256 = "59536e6ae64359b716ba9c46c39183403b01eabfbd57578e84398b4829ca499a",
    strip_prefix = "rules_docker-0.22.0",
    urls = ["https://github.com/bazelbuild/rules_docker/releases/download/v0.22.0/rules_docker-v0.22.0.tar.gz"],
)

load("@io_bazel_rules_docker//repositories:repositories.bzl",
     container_repositories = "repositories")
container_repositories()

load("@io_bazel_rules_docker//repositories:deps.bzl", container_deps = "deps")
container_deps()

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
