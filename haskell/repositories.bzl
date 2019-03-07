"""Workspace rules (repositories)"""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

def haskell_repositories(
        bazel_skylib = True,
        io_tweag_rules_nixpkgs = False):
    """Provide all repositories that are necessary for `rules_haskell` to
    function.
    """
    excludes = native.existing_rules().keys()

    if ("bazel_skylib" not in excludes) and bazel_skylib:
        http_archive(
            name = "bazel_skylib",
            sha256 = "eb5c57e4c12e68c0c20bc774bfbc60a568e800d025557bc4ea022c6479acc867",
            strip_prefix = "bazel-skylib-0.6.0",
            urls = ["https://github.com/bazelbuild/bazel-skylib/archive/0.6.0.tar.gz"],
        )

    if ("io_tweag_rules_nixpkgs" not in excludes) and io_tweag_rules_nixpkgs:
        rules_nixpkgs_version = "0.5.2"
        rules_nixpkgs_version_is_hash = False
        rules_nixpkgs_sha256 = "5a384daa57b49abf9f0b672852f1a66a3c52aecf9d4d2ac64f6de0fd307690c8"

        http_archive(
            name = "io_tweag_rules_nixpkgs",
            sha256 = rules_nixpkgs_sha256,
            strip_prefix = "rules_nixpkgs-%s" % rules_nixpkgs_version,
            urls = ["https://github.com/tweag/rules_nixpkgs/archive/%s.tar.gz" % rules_nixpkgs_version] if rules_nixpkgs_version_is_hash else ["https://github.com/tweag/rules_nixpkgs/archive/v%s.tar.gz" % rules_nixpkgs_version],
        )
