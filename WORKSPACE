workspace(name = "io_tweag_rules_haskell_examples")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

local_repository(
    name = "tutorial",
    path = "tutorial",
)

http_archive(
    name = "io_tweag_rules_haskell",
    strip_prefix = "rules_haskell-0.8",
    urls = ["https://github.com/tweag/rules_haskell/archive/v0.8.tar.gz"],
    sha256 = "431d492a8ee6a110cdf42496181c9d27225dfb997379e64a148eb8e69f272ab7",
)

load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")

haskell_repositories()

http_archive(
    name = "io_tweag_rules_nixpkgs",
    strip_prefix = "rules_nixpkgs-0.5.2",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.5.2.tar.gz"],
    sha256 = "5a384daa57b49abf9f0b672852f1a66a3c52aecf9d4d2ac64f6de0fd307690c8",
)

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_git_repository",
    "nixpkgs_package",
)

nixpkgs_git_repository(
    name = "nixpkgs",
    revision = "18.09",
    sha256 = "6451af4083485e13daa427f745cbf859bc23cb8b70454c017887c006a13bd65e",
)

nixpkgs_package(
    name = "ghc",
    # For rts example. Not needed if you're not using the RTS directly.
    build_file = "@io_tweag_rules_haskell//haskell:ghc.BUILD",
    # For vector example. Just use `attribute_path = haskell.compiler.ghc822`
    # when no extra packages needed.
    nix_file_content = """
  let pkgs = import <nixpkgs> {}; in
  pkgs.haskell.packages.ghc822.ghcWithPackages (p: with p;
    [primitive semigroupoids]
  )
  """,
    repository = "@nixpkgs",
)

register_toolchains("//:ghc")
