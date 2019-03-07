{-# OPTIONS -Wall #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Foldable (for_)
import Data.List (isInfixOf)

import Test.Hspec.RulesHaskell
  ( hspec, it, describe
  , assertSuccess, assertFailure, outputSatisfy
  , safeShell, bazel, bazelQuery)

main :: IO ()
main = hspec $ do
  it "bazel lint" $ do
    assertSuccess (bazel ["run", "//:buildifier"])

  it "bazel test" $ do
    assertSuccess (bazel ["test", "//...", "--build_tests_only"])

  it "haddock links" $ do
    -- Test haddock links
    -- All haddock tests are stored inside //tests/haddock
    -- Temporaries files appears inside /doc-.... outputs and are ignored

    -- the copy / chmod is here to workaround the fact that
    -- linkchecker is dropping privileges to "nobody" user if called
    -- from root, which is the case on CI.
    assertSuccess (safeShell
      [ "bazel build --config=ci //tests/haddock/..."
      , "pwd=$(pwd)"
      , "cd $(mktemp -d)"
      , "cp -r $pwd/bazel-ci-bin/tests/haddock ."
      , "chmod -R o+r ."
      , "linkchecker . --ignore-url=/doc-"
      ])

  it "bazel test prof" $ do
    assertSuccess (bazel ["test", "-c", "dbg", "//...", "--build_tests_only"])

  describe "repl" $ do
    it "for libraries" $ do
      -- Test whether building of repl forces all runtime dependencies by itself:
      -- TODO(Profpatsch) remove clean once repl uses runfiles
      -- because otherwise the test runner succeeds if the first test fails
      assertSuccess (bazel ["clean"])

      assertSuccess (bazel ["run", "//tests/repl-targets:hs-lib@repl", "--", "-ignore-dot-ghci", "-e", "show (foo 10) ++ bar ++ baz ++ gen"])
      assertSuccess (bazel ["run", "//tests/repl-targets:hs-lib-bad@repl", "--", "-ignore-dot-ghci", "-e", "1 + 2"])

    it "for binaries" $ do
      -- Test whether building of repl forces all runtime dependencies by itself:
      -- TODO(Profpatsch) remove clean once repl uses runfiles
      assertSuccess (bazel ["clean"])

      assertSuccess (bazel ["run", "//tests/repl-targets:hs-bin@repl", "--", "-ignore-dot-ghci", "-e", ":main"])

      assertSuccess (bazel ["run", "//tests/binary-indirect-cbits:binary-indirect-cbits@repl", "--", "-ignore-dot-ghci", "-e", ":main"])

    -- Test `compiler_flags` from toolchain and rule for REPL
    it "compiler flags" $ do
      assertSuccess (bazel ["run", "//tests/repl-flags:compiler_flags@repl", "--", "-ignore-dot-ghci", "-e", ":main"])

    -- Test `repl_ghci_args` from toolchain and rule for REPL
    it "repl flags" $ do
      assertSuccess (bazel ["run", "//tests/repl-flags:repl_flags@repl", "--", "-ignore-dot-ghci", "-e", "foo"])

  it "startup script" $ do
    assertSuccess (safeShell [
        "pwd=$(pwd)"
      , "cd $(mktemp -d)"
      , "$pwd/start"

      -- Copy the bazel configuration, this is only useful for CI
      , "mkdir tools"
      , "cp $pwd/.bazelrc .bazelrc"

      -- Set Nixpkgs in environment variable to avoid hardcoding it in
      -- start script itself
      , "NIX_PATH=nixpkgs=$pwd/nixpkgs/default.nix \
        \bazel fetch \
        \--config=ci \
        \--override_repository=io_tweag_rules_haskell=$pwd \
        \//..."
      ])

  describe "failures" $ do
    all_failure_tests <- bazelQuery "kind(rule, //tests/failures/...) intersect attr('tags', 'manual', //tests/failures/...)"

    for_ all_failure_tests $ \test -> do
      it test $ do
        assertFailure (bazel ["build", "test"])

  -- Test that the repl still works if we shadow some Prelude functions
  it "repl name shadowing" $ do
    outputSatisfy p (bazel ["run", "//tests/repl-name-conflicts:lib@repl", "--", "-ignore-dot-ghci", "-e", "stdin"])
      where
        p (stdout, stderr) = not $ any ("error" `isInfixOf`) [stdout, stderr]
