{ pkgs ? import ./nixpkgs.nix {} }:

with pkgs;
with darwin.apple_sdk.frameworks;

let

  /* with pkgs; */
  /* with darwin.apple_sdk.frameworks; */
  # XXX On Darwin, workaround
  # https://github.com/NixOS/nixpkgs/issues/42059. See also
  # https://github.com/NixOS/nixpkgs/pull/41589.
  cc = runCommand "cc-wrapper-bazel" {
      buildInputs = [ stdenv.cc makeWrapper libiconv ];
    }
    ''
      mkdir -p $out/bin
      # Copy the content of stdenv.cc
      for i in ${stdenv.cc}/bin/*
      do
        ln -sf $i $out/bin
      done
      # Override clang
      rm $out/bin/clang
      makeWrapper ${stdenv.cc}/bin/clang $out/bin/clang \
        --add-flags "-isystem ${llvmPackages.libcxx}/include/c++/v1 \
                     -F${CoreFoundation}/Library/Frameworks \
                     -F${CoreServices}/Library/Frameworks \
                     -F${Security}/Library/Frameworks \
                     -F${Foundation}/Library/Frameworks \
                     -L${libcxx}/lib \
                     -L${libiconv}/lib \
                     -L${darwin.libobjc}/lib"
   '';
  mkShell = pkgs.mkShell.override {
     stdenv = with pkgs; if stdenv.isDarwin then overrideCC stdenv cc else stdenv;
  };
in
mkShell {
  BAZEL_USE_CPP_ONLY_TOOLCHAIN=1;
  buildInputs = [
    nix
    python
    which
    bazel
  ];
}
