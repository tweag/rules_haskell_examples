import Test.Hspec.RulesHaskell
  ( hspec, it, assertSuccess, bazel)


main :: IO ()
main = hspec $ do
  it "bazel build all" $ do
    assertSuccess (bazel ["build", "//..."])

  it "bazel test" $ do
    assertSuccess (bazel ["test", "//..."])

  it "bazel build tutorial repository" $ do
    assertSuccess (bazel ["build", "@tutorial//..."])

  it "bazel test tutorial repository" $ do
    assertSuccess (bazel ["test", "@tutorial//..."])
