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
    sha256 = "c15ef66698f5d2122a3e875c327d9ecd34a231a9dc4753b9500e70518464cc21",
    strip_prefix = "rules_docker-7da0de3d094aae5601c45ae0855b64fb2771cd72",
    urls = ["https://github.com/bazelbuild/rules_docker/archive/7da0de3d094aae5601c45ae0855b64fb2771cd72.tar.gz"],
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
