# rule_haskell examples

Examples of using [rules_haskell][rules_haskell], the Bazel rule set
for building Haskell code.

* [**vector:**](./vector/) shows how to build the `vector` package as
  found on Hackage, using a [Nix](https://nixos.org/nix/) provided compiler toolchain.
* [**rts:**](./rts/) demonstrates foreign exports and shows how to
  link against GHC's RTS library, i.e. `libHSrts.so`.
* [**tutorial:**](./tutorial/) a separate workspace for the [tutorial][tutorial].
* [**cat_hs:**](./cat_hs/) a separate workspace for a [Hazel][hazel] example project.

## Root Workspace

We use `nix-shell` from the Nix package manager to pin `bazel`,
`rules_haskell` and `gcc` to a tested configuration. You are welcome to
try your native versions of these tools if they happen to be compatible
as well.

Build everything in the root workspace with;

```
$ nix-shell --run "bazel build //..."
```

Show every target in this workspace;

```
$ nix-shell --run "bazel query //..."
//vector:vector
//vector:semigroups
//vector:primitive
//vector:ghc-prim
//vector:deepseq
//vector:base
//rts:add-one
//rts:add-one-so
//rts:add-one-hs
//rts:base
//:ghc-impl
//:ghc
```

Build the two main Haskell targets;

```
$ nix-shell --run "bazel build //vector:vector"
$ bazel build //rts:add-one-hs
```

## Tutorial Workspace

Build everything in the tutorial workspace with;

```
$ nix-shell --run "bazel build @tutorial//..."
```

Show everything in the tutorial;

```
$ nix-shell --run "bazel query @tutorial//..."
@tutorial//main:demorgan
@tutorial//main:base
@tutorial//lib:booleans
@tutorial//:ghc-impl
@tutorial//:ghc
```

Build and run the tutorial example;

```
$ nix-shell --run "bazel build @tutorial//lib:booleans"
$ nix-shell --run "bazel build "@tutorial//main:demorgan"
$ nix-shell --run "bazel run "@tutorial//main:demorgan"
```

## cat_hs - Hazel example

Change into the `cat_hs` directory and follow the instructions given in the
[README file](./cat_hs/README.md).

[rules_haskell]: https://github.com/tweag/rules_haskell
[tutorial]: https://rules-haskell.readthedocs.io
[hazel]: https://github.com/FormationAI/hazel
